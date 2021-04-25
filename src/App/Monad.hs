{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Monad
  ( AppM,
    Env (..),
    runAppWith,
  )
where

import App.DB (Pool, WithPool, getPool)
import App.Log (Log)
import App.Metrics (AppMetrics, WithMetrics, getMetrics)
import Chronos (Time)
import Colog (HasLog, LogAction, Message, getLogAction, setLogAction)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader (MonadReader (ask), ReaderT, asks, runReaderT)
import UnliftIO (MonadUnliftIO, withRunInIO)

newtype AppM f a = AppM
  { runApp :: ReaderT (Env f) IO a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader (Env f), MonadRandom)

instance MonadUnliftIO (AppM f) where
  withRunInIO go = do
    env <- ask
    liftIO $ go $ runAppWith env

data Env f = Env
  { envDBPool :: Pool,
    envMetrics :: AppMetrics f,
    envLog :: Log (AppM f),
    envStartupTime :: Time
  }

instance WithPool (AppM f) where
  getPool = asks envDBPool

instance WithMetrics (AppM f) f where
  getMetrics = asks envMetrics

instance HasLog (Env f) Message (AppM f) where
  getLogAction :: Env f -> LogAction (AppM f) Message
  getLogAction = envLog
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction (AppM f) Message -> Env f -> Env f
  setLogAction newLogAction env = env {envLog = newLogAction}
  {-# INLINE setLogAction #-}

runAppWith :: Env f -> AppM f a -> IO a
runAppWith e a = runReaderT (runApp a) e
