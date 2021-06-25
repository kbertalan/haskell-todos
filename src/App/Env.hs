{-# LANGUAGE MultiParamTypeClasses #-}

module App.Env where

import App.DB (Pool)
import App.Log (LogAction)
import App.Metrics (AppMetrics)
import Chronos (Time)
import Data.Has (Has (obtain), Over (over))

data Env metricsF logMessage m = Env
  { envDBPool :: Pool,
    envMetrics :: AppMetrics metricsF,
    envLog :: LogAction m logMessage,
    envStartupTime :: Time
  }

instance Has Pool (Env f l m) where obtain = envDBPool

instance Has (AppMetrics f) (Env f l m) where obtain = envMetrics

instance Has (LogAction m l) (Env f l m) where obtain = envLog

instance Over (LogAction m l) (LogAction m other) (Env f l m) (Env f other m) where
  over f env =
    env
      { envLog = f $ envLog env
      }

instance Has Time (Env f l m) where obtain = envStartupTime
