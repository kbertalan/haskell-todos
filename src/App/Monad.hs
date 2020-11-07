{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Monad
  ( AppM
  , Env(..)
  , runAppWith
  ) where

import Colog
import Control.Monad.Random.Class
import Control.Monad.Reader

import App.DB                     (DB, WithDB, getDB)
import App.Ekg                    (Ekg, WithEkg, getEkg)
import App.Log                    (Log)

newtype AppM a = AppM
  { runApp :: ReaderT (Env AppM) IO a
  } deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader (Env AppM), MonadRandom)

data Env m = Env
  { envDB  :: DB
  , envEkg :: Ekg
  , envLog :: Log m
  }

instance WithDB AppM where
  getDB = asks envDB

instance WithEkg AppM where
  getEkg = asks envEkg

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLog
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLog = newLogAction }
  {-# INLINE setLogAction #-}

runAppWith :: Env AppM -> AppM a -> IO a
runAppWith e a = runReaderT (runApp a) e

