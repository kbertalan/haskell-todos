{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module App where

import App.DB as DB (Options, migrate, runWithPool)
import App.Env (Env (..))
import App.Log as Log (logInfo, runWithLog)
import App.Metrics as Metrics (AppMetrics (metricsEndpoint, metricsMiddleware), Options, runWithMetrics)
import App.Monad (runAppWith)
import App.Random as Random (Options, configure)
import App.Web as Web (Options, run, webApp)
import Aws.Lambda (HandlerName (HandlerName), defaultDispatcherOptions)
import Aws.Lambda.Wai (runWaiAsProxiedHttpLambda)
import Chronos (Time (getTime), now)
import Colog (usingLoggerT)
import Data.HKD (TraversableHKD (traverseHKD))
import Data.Has (Has (obtain))
import qualified Data.Text as T
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
  startup opts $ Web.run (web opts)

lambda :: App.Options -> IO ()
lambda opts =
  startup opts $
    runWaiAsProxiedHttpLambda defaultDispatcherOptions Nothing (HandlerName "todos") . pure

type Callback = Application -> IO ()

startup :: App.Options -> Callback -> IO ()
startup opts callback =
  now >>= \time ->
    runWithLog $ \log ->
      let exposedMetrics = AllMetrics Todo.metrics
       in runWithMetrics (metrics opts) exposedMetrics $ \ms ->
            runWithPool (db opts) $ \pool -> do
              Random.configure $ random opts
              migrate pool

              let env = Env pool ms log time
              currentTime <- now
              runAppWith env $
                usingLoggerT log $
                  logInfo $ "Started up in " <> T.pack (printf "%.4f" (diffTimeInSeconds currentTime time)) <> "s"

              callback $
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
