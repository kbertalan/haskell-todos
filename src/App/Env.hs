{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Env where

import App.DB (Pool)
import App.Log (Log)
import App.Metrics (AppMetrics)
import Chronos (Time)
import Colog (HasLog, LogAction, Message, getLogAction, setLogAction)
import Data.Has (Has (obtain))

data Env f m = Env
  { envDBPool :: Pool,
    envMetrics :: AppMetrics f,
    envLog :: Log m,
    envStartupTime :: Time
  }

instance HasLog (Env f m) Message m where
  getLogAction :: Env f m -> LogAction m Message
  getLogAction = envLog
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env f m -> Env f m
  setLogAction newLogAction env = env {envLog = newLogAction}
  {-# INLINE setLogAction #-}

instance Has Pool (Env f m) where obtain = envDBPool

instance Has (AppMetrics f) (Env f m) where obtain = envMetrics

instance Has (Log m) (Env f m) where obtain = envLog

instance Has Time (Env f m) where obtain = envStartupTime
