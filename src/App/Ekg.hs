{-# LANGUAGE RecordWildCards #-}

module App.Ekg
  ( Options (..),
    Ekg,
    WithEkg,
    getEkg,
    runWithEkg,
    serverMetricsMiddleware,
  )
where

import Control.Concurrent (killThread)
import Control.Exception (bracket)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (Text, toStrict)
import Network.Wai (Middleware)
import Network.Wai.Metrics (metrics, registerWaiMetrics)
import System.Remote.Monitoring as EKG (Server, forkServer, serverMetricStore, serverThreadId)

data Options = Options
  { host :: !Text,
    port :: !Int
  }
  deriving (Show)

type Ekg = Server

runWithEkg :: Options -> (Ekg -> IO ()) -> IO ()
runWithEkg Options {..} =
  bracket
    (EKG.forkServer (encodeUtf8 $ toStrict host) port)
    (killThread . serverThreadId)

serverMetricsMiddleware :: Ekg -> IO Middleware
serverMetricsMiddleware ekg =
  metrics <$> registerWaiMetrics (serverMetricStore ekg)

class WithEkg m where
  getEkg :: m Ekg
