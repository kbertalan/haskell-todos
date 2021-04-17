{-# LANGUAGE RecordWildCards #-}

module App.Metrics
  ( Options (..),
    Metrics (..),
    WithMetrics,
    getMetrics,
    runWithMetrics,
  )
where

import           Control.Monad                                  (forM_)
import           Control.Monad.Reader                           (ReaderT (runReaderT))
import           Data.Text                                      (Text)
import qualified Data.Text                                      as T
import           Network.HTTP.Types                             (methodGet)
import           Network.HTTP.Types.Header                      (hContentType)
import           Network.HTTP.Types.Status                      (status200)
import           Network.Wai                                    (Middleware, Request (requestMethod), responseBuilder)
import           Network.Wai.Internal                           (Request (pathInfo))
import           Network.Wai.Middleware.Prometheus              (applicationMetrics, instrumentApplication)
import           System.Metrics.Prometheus.Concurrent.Registry  (Registry, new, sample)
import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT (unRegistryT), registerCounter',
                                                                 registerGauge', registerHistogram')
import           System.Metrics.Prometheus.Encode.Text          (encodeMetrics)
import           System.Metrics.Prometheus.Metric               (Metric (CounterMetric, GaugeMetric, HistogramMetric))
import           System.Metrics.Prometheus.MetricId             (MetricId (labels, name))
import           System.Metrics.Prometheus.Registry             (RegistrySample)

newtype Options = Options
  { path :: Text
  }
  deriving (Show)

data Metrics = Metrics
  { metricsRegistry   :: !Registry,
    metricsMiddleware :: !Middleware,
    metricsEndpoint   :: !Middleware
  }

type ExposedMetrics = [(MetricId, Metric)]

runWithMetrics :: Options -> ExposedMetrics -> (Metrics -> IO ()) -> IO ()
runWithMetrics Options {..} exposed action = do
  registry <- new
  runRegistryT' registry $ do
    forM_ exposed $ \(mid, m) ->
      let n = name mid
          l = labels mid
       in case m of
            CounterMetric c   -> registerCounter' n l c
            GaugeMetric g     -> registerGauge' n l g
            HistogramMetric h -> registerHistogram' n l h
  appMetrics <- runRegistryT' registry $ applicationMetrics mempty

  let endpoint = prometheusEndpoint (filter (not . T.null) $ T.split (=='/') path) $ sample registry

  action $ Metrics registry (instrumentApplication appMetrics) endpoint

runRegistryT' :: Registry -> RegistryT m a -> m a
runRegistryT' registry r = runReaderT (unRegistryT r) registry

class WithMetrics m where
  getMetrics :: m Metrics

prometheusEndpoint :: [Text] -> IO RegistrySample -> Middleware
prometheusEndpoint path runSample app request respond
    | matches path request = respond . prometheusResponse =<< runSample
    | otherwise = app request respond
  where
    prometheusResponse = responseBuilder status200 headers . encodeMetrics
    headers = [(hContentType, "text/plain; version=0.0.4")]

matches :: [Text] -> Request -> Bool
matches path request = matchesMethod && matchesPath
  where
    matchesPath = pathInfo request == path
    matchesMethod = requestMethod request == methodGet
