module App
  ( AppM
  , Env(..)
  , runAppWith
  ) where

import Control.Monad.Reader
import Control.Monad.Random.Class

import App.DB (DB, WithDB, getDB)
import App.Ekg (Ekg, WithEkg, getEkg)

newtype AppM a = AppM
  { runApp :: ReaderT Env IO a
  } deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env, MonadRandom)

data Env = Env
  { envDB :: DB
  , envEkg :: Ekg
  }

instance WithDB AppM where
  getDB = asks envDB

instance WithEkg AppM where
  getEkg = asks envEkg

runAppWith :: Env -> AppM a -> IO a
runAppWith e a = runReaderT (runApp a) e

