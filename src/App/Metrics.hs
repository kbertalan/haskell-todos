{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module App.Metrics
  ( Options (..),
    AppMetrics (..),
    runWithMetrics,
    timed,
  )
where

import Chronos (Timespan (getTimespan), stopwatch)
import Control.DeepSeq (NFData, force)
import Control.Monad.Identity (Identity)
import Data.HKD (TraversableHKD (traverseHKD))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (methodGet)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Middleware, Request (requestMethod), responseBuilder)
import Network.Wai.Internal (Request (pathInfo))
import Network.Wai.Middleware.Prometheus (applicationMetrics, instrumentApplication)
import System.Metrics.Prometheus.Concurrent.RegistryT
import System.Metrics.Prometheus.Encode.Text (encodeMetrics)
import System.Metrics.Prometheus.Metric.Histogram (Histogram, observe)
import System.Metrics.Prometheus.Registry (RegistrySample)
import UnliftIO (MonadUnliftIO, withRunInIO)

newtype Options = Options
  { path :: Text
  }
  deriving (Show)

data AppMetrics f = AppMetrics
  { metricsMiddleware :: !Middleware,
    metricsEndpoint :: !Middleware,
    metricsRegistered :: !(f Identity)
  }

runWithMetrics :: (TraversableHKD f) => Options -> f (RegistryT IO) -> (AppMetrics f -> IO a) -> IO a
runWithMetrics Options {..} exposed action = do
  (instrumentMiddleware, endpointMiddleware, registeredMetrics) <- runRegistryT $ do
    appMetrics <- applicationMetrics mempty
    endpoint <- prometheusEndpoint pathAsList =<< sample
    registered <- traverseHKD register exposed

    return (instrumentApplication appMetrics, endpoint, registered)

  action $ AppMetrics instrumentMiddleware endpointMiddleware registeredMetrics
  where
    register :: RegistryT IO a -> RegistryT IO (Identity a)
    register = fmap return
    pathAsList = filter (not . T.null) $ T.split (== '/') path

prometheusEndpoint :: [Text] -> IO RegistrySample -> RegistryT IO Middleware
prometheusEndpoint path runSample = return go
  where
    go app request respond
      | matches path request = respond . prometheusResponse =<< runSample
      | otherwise = app request respond
    prometheusResponse = responseBuilder status200 headers . encodeMetrics
    headers = [(hContentType, "text/plain; version=0.0.4")]

matches :: [Text] -> Request -> Bool
matches path request = matchesMethod && matchesPath
  where
    matchesPath = pathInfo request == path
    matchesMethod = requestMethod request == methodGet

timed ::
  MonadUnliftIO m =>
  NFData a =>
  Histogram ->
  m a ->
  m a
timed histogram action = do
  withRunInIO $ \run -> do
    (duration, result) <-
      stopwatch $
        force <$> run action
    observe (asMillisecond duration) histogram
    return result
  where
    asMillisecond = (/ 1_000_000) . fromIntegral . getTimespan
