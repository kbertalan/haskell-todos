{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module App where

import App.DB as DB (Options, migrate, runWithPool)
import App.Env (Env (..))
import App.Log as Log (runWithDisabledLog, runWithLog)
import App.Metrics as Metrics (AppMetrics (metricsEndpoint, metricsMiddleware), Options, runWithMetrics)
import App.Monad (runAppWith)
import App.Random as Random (Options, configure)
import App.Web as Web (Options, run, webApp)
import Chronos (Time (getTime), now)
import Data.HKD (TraversableHKD (traverseHKD))
import Data.Has (Has (obtain))
import Health
import Network.Wai (Application)
import Servant ((:<|>) (..))
import Text.Printf (printf)
import Todo (TodoApi, todoApi)
import qualified Todo
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
      let exposedMetrics = AllMetrics Todo.metrics
       in runWithMetrics (metrics opts) exposedMetrics $ \ms ->
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
    runWithDisabledLog $ \log ->
      let exposedMetrics = AllMetrics Todo.metrics
       in runWithMetrics (metrics opts) exposedMetrics $ \ms ->
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

newtype AllMetrics m = AllMetrics
  { todo :: Todo.Metrics m
  }

instance TraversableHKD AllMetrics where
  traverseHKD f (AllMetrics todo) = AllMetrics <$> traverseHKD f todo

instance Has (Todo.Metrics m) (AllMetrics m) where
  obtain = todo
