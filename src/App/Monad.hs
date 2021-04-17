{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Monad
  ( AppM,
    Env (..),
    runAppWith,
  )
where

import App.DB (Pool, WithPool, getPool)
import App.Metrics (Metrics, WithMetrics, getMetrics)
import App.Log (Log)
import Chronos (Time)
import Colog (HasLog, LogAction, Message, getLogAction, setLogAction)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader (MonadReader (ask), ReaderT, asks, runReaderT)
import UnliftIO (MonadUnliftIO, withRunInIO)

newtype AppM a = AppM
  { runApp :: ReaderT Env IO a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env, MonadRandom)

instance MonadUnliftIO AppM where
  withRunInIO go = do
    env <- ask
    liftIO $ go $ runAppWith env

data Env = Env
  { envDBPool :: Pool,
    envMetrics :: Metrics,
    envLog :: Log AppM,
    envStartupTime :: Time
  }

instance WithPool AppM where
  getPool = asks envDBPool

instance WithMetrics AppM where
  getMetrics = asks envMetrics

instance HasLog Env Message AppM where
  getLogAction :: Env -> LogAction AppM Message
  getLogAction = envLog
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction AppM Message -> Env -> Env
  setLogAction newLogAction env = env {envLog = newLogAction}
  {-# INLINE setLogAction #-}

runAppWith :: Env -> AppM a -> IO a
runAppWith e a = runReaderT (runApp a) e
