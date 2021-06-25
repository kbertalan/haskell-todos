{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Env where

import App.DB (Pool)
import App.Log (Log)
import App.Metrics (AppMetrics)
import Chronos (Time)
import Data.Has (Has (obtain))

data Env f m = Env
  { envDBPool :: Pool,
    envMetrics :: AppMetrics f,
    envLog :: Log m,
    envStartupTime :: Time
  }

instance Has Pool (Env f m) where obtain = envDBPool

instance Has (AppMetrics f) (Env f m) where obtain = envMetrics

instance Has (Log m) (Env f m) where obtain = envLog

instance Has Time (Env f m) where obtain = envStartupTime
