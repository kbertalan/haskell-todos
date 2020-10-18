{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module App
  ( app
  , App.Options(..)
  ) where

import Data.Aeson.Types as A
import Data.Text.Lazy as T
import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import Hasql.Migration as M
import Hasql.Pool as Pool
import Hasql.Transaction.Sessions as T
import Network.HTTP.Types.Status
import Web.Scotty as S

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

    scotty (port opts) $ do
      defaultHandler $ \msg -> do
        liftIO $ print msg
        status status500
        S.json $ App.Error msg

      healthApi pool
      todoApi pool

migrateDatabase :: Pool -> IO (Either UsageError ())
migrateDatabase pool = use pool $ do
  migrations <- liftIO $ M.loadMigrationsFromDirectory "./migrations"
  forM_ (M.MigrationInitialization : migrations) $ \m ->
    T.transaction T.Serializable T.Write $ M.runMigration m

newtype Error = Error
  { message :: Text
  } deriving stock (Generic)
  deriving anyclass (ToJSON)

