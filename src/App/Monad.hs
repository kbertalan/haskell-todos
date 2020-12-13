{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Monad
  ( AppM
  , Env(..)
  , runAppWith
  ) where

import Colog                      (HasLog, LogAction, Message, getLogAction, setLogAction)
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader       (MonadIO, MonadReader, ReaderT, asks, runReaderT)

import App.DB                     (DB, WithDB, getDB)
import App.Ekg                    (Ekg, WithEkg, getEkg)
import App.Log                    (Log)
import App.Time                   (Time)

newtype AppM a = AppM
  { runApp :: ReaderT Env IO a
  } deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env, MonadRandom)

data Env = Env
  { envDB          :: DB
  , envEkg         :: Ekg
  , envLog         :: Log AppM
  , envStartupTime :: Time
  }

instance WithDB AppM where
  getDB = asks envDB

instance WithEkg AppM where
  getEkg = asks envEkg

instance HasLog Env Message AppM where
  getLogAction :: Env -> LogAction AppM Message
  getLogAction = envLog
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction AppM Message -> Env -> Env
  setLogAction newLogAction env = env { envLog = newLogAction }
  {-# INLINE setLogAction #-}

runAppWith :: Env -> AppM a -> IO a
runAppWith e a = runReaderT (runApp a) e

