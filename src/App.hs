{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module App
  ( app
  , App.Options(..)
  ) where

import Data.Aeson.Types as A
import Data.Text.Lazy as T
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad.Reader
import GHC.Generics (Generic)
import Hasql.Migration as M
import Hasql.Pool as Pool
import Hasql.Transaction.Sessions as T
import Network.HTTP.Types.Status
import Web.Scotty.Trans as S

import Env
import Health (healthApi)
import Todo (todoApi)

data Options = Options
  { port :: !Int
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
    scottyT (port opts) (runAppWith env) routeDefs

migrateDatabase :: Pool -> IO (Either UsageError ())
migrateDatabase pool = use pool $ do
  migrations <- liftIO $ M.loadMigrationsFromDirectory "./migrations"
  forM_ (M.MigrationInitialization : migrations) $ \m ->
    T.transaction T.Serializable T.Write $ M.runMigration m

routeDefs :: ScottyT Text AppM ()
routeDefs = do
  defaultHandler $ \msg -> do
    liftIO $ print msg
    status status500
    S.json $ App.Error msg

  healthApi
  todoApi


newtype Error = Error
  { message :: Text
  } deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype AppM a = AppM
  { runApp :: ReaderT Env IO a
  } deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env)

instance WithPool AppM where
  usePool session = ask >>= liftIO . flip Pool.use session . envPool

runAppWith :: Env -> AppM a -> IO a
runAppWith e a = runReaderT (runApp a) e

