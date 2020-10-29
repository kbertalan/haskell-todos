module App
  ( App.run
  , App.Options(..)
  ) where

import Control.Monad.Reader

import App.DB as DB
import App.Ekg as Ekg
import App.Web as Web

import Health (healthApi)
import Todo (todoApi)

data Options = Options
  { web :: !Web.Options
  , db :: !DB.Options
  , ekg :: !Ekg.Options
  }
  deriving (Show)

run :: App.Options -> IO ()
run opts =
  Ekg.runWithEkg (ekg opts) $ \ekg ->
    DB.runWithDB (db opts) $ \db -> do
      DB.migrate db >>= \case
        Right _ -> return ()
        Left e -> error $ show e

      let env = Env db ekg
      Web.run (web opts) (runAppWith env) $ do
        healthApi
        todoApi

runAppWith :: Env -> AppM a -> IO a
runAppWith e a = runReaderT (runApp a) e

newtype AppM a = AppM
  { runApp :: ReaderT Env IO a
  } deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env)

data Env = Env
  { envDB :: DB
  , envEkg :: Ekg
  }

instance WithDB AppM where
  getDB = asks envDB

instance WithEkg AppM where
  getEkg = asks envEkg

