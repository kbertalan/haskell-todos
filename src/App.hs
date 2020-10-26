module App
  ( app
  , App.Options(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader

import App.DB as DB
import App.Web as Web
import Env
import Health (healthApi)
import Todo (todoApi)

data Options = Options
  { web :: !Web.Options
  , db :: !DB.Options
  }
  deriving (Show)

app :: App.Options -> IO ()
app opts = do
  DB.runWithDB (db opts) $ \db -> do
    DB.migrate db >>= \case
      Right _ -> return ()
      Left e -> error $ show e

    let env = Env db
    Web.run (web opts) (runAppWith env) $ do
      healthApi
      todoApi

newtype AppM a = AppM
  { runApp :: ReaderT Env IO a
  } deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env)

runAppWith :: Env -> AppM a -> IO a
runAppWith e a = runReaderT (runApp a) e

instance WithEnv AppM where
  getEnv = ask

instance WithDB AppM where
  getDB = envDB <$> getEnv

