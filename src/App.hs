{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module App where

import App.DB as DB (Options, migrate, runWithPool)
import App.Log as Log (runWithLog)
import App.Metrics as Metrics (AppMetrics (metricsEndpoint, metricsMiddleware), Options, runWithMetrics)
import App.Monad (Env (Env), runAppWith)
import App.Random as Random (Options, configure)
import App.Web as Web (Options, run, webApp)
import Chronos (Time (getTime), now)
import Data.HKD (FunctorHKD (mapHKD), TraversableHKD (traverseHKD))
import Health
import Network.Wai (Application)
import Servant ((:<|>) (..))
import Text.Printf (printf)
import Todo (TodoApi, todoApi)
import qualified Todo
import Todo.Metrics (WithTodoMetrics)
import Prelude hiding (log)

data Options = Options
  { web :: !Web.Options,
    db :: !DB.Options,
    metrics :: !Metrics.Options,
    random :: !Random.Options
  }
  deriving (Show)

type API = HealthApi :<|> TodoApi

run :: App.Options -> IO ()
run opts =
  now >>= \time ->
    runWithLog $ \log ->
      let registerMetrics = App.Metrics Todo.metrics
       in runWithMetrics (metrics opts) registerMetrics $ \ms ->
            runWithPool (db opts) $ \pool -> do
              Random.configure $ random opts
              migrate pool

              let env = Env pool ms log time
              currentTime <- now
              putStrLn $ "Started up in " <> printf "%.4f" (diffTimeInSeconds currentTime time) <> "s"

              Web.run @API
                (web opts)
                [metricsEndpoint ms, metricsMiddleware ms]
                (healthApi :<|> todoApi)
                (runAppWith env)

lambda :: App.Options -> IO Application
lambda opts =
  now >>= \time ->
    runWithLog $ \log ->
      let registerMetrics = App.Metrics Todo.metrics
       in runWithMetrics (metrics opts) registerMetrics $ \ms ->
            runWithPool (db opts) $ \pool -> do
              Random.configure $ random opts
              migrate pool

              let env = Env pool ms log time
              pure $
                Web.webApp @API
                  [metricsEndpoint ms, metricsMiddleware ms]
                  (healthApi :<|> todoApi)
                  (runAppWith env)

diffTimeInSeconds :: Time -> Time -> Double
diffTimeInSeconds h l = fromIntegral (getTime h - getTime l) / nanoSecondInSecond
  where
    nanoSecondInSecond = 1_000_000_000

newtype Metrics m = Metrics
  { todo :: Todo.Metrics m
  }

instance FunctorHKD App.Metrics where
  mapHKD f (Metrics todo) = Metrics $ mapHKD f todo

instance TraversableHKD App.Metrics where
  traverseHKD f (Metrics todo) = Metrics <$> traverseHKD f todo

instance WithTodoMetrics App.Metrics where
  getTodoMetrics = todo
