{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module App
  ( app
  , App.Options(..)
  ) where

import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Hasql.Migration as M
import Hasql.Pool as Pool
import Hasql.Transaction.Sessions as T

import App.DB as DB
import App.Web as Web
import Env
import Health (healthApi)
import Todo (todoApi)

data Options = Options
  { web :: !Web.Options
  , db :: Pool.Settings
  }
  deriving (Show)

app :: App.Options -> IO ()
app opts = do
  bracket (Pool.acquire $ db opts) Pool.release $ \pool -> do
    migrateDatabase pool >>= \case
      Right _ -> return ()
      Left e -> error $ show e

    let env = Env pool
    Web.run (web opts) (runAppWith env) $ do
      healthApi
      todoApi

migrateDatabase :: DB.Pool -> IO (PoolResult ())
migrateDatabase pool = use pool $ do
  migrations <- liftIO $ M.loadMigrationsFromDirectory "./migrations"
  forM_ (M.MigrationInitialization : migrations) $ \m ->
    T.transaction T.Serializable T.Write $ M.runMigration m

newtype AppM a = AppM
  { runApp :: ReaderT Env IO a
  } deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env)

runAppWith :: Env -> AppM a -> IO a
runAppWith e a = runReaderT (runApp a) e

instance WithEnv AppM where
  getEnv = ask

instance WithPool AppM where
  getPool = envPool <$> getEnv

instance UsePool AppM where
  usePool session = getPool >>= liftIO . flip Pool.use session

