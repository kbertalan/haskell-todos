{-# LANGUAGE RecordWildCards #-}

module App.DB
  ( Options(..)
  , WithDB
  , getDB
  , DB
  , runWithDB
  , Result
  , Statement
  , S.statement
  , run
  , migrate
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Exception (bracket)
import Data.String (fromString)
import qualified Hasql.Connection as C
import qualified Hasql.Pool as P
import qualified Hasql.Session as S
import qualified Hasql.Migration as M
import qualified Hasql.Transaction.Sessions as T

data Options = Options
  { poolSize :: !Int
  , poolTimeout :: !Int
  , dbHost :: !String
  , dbPort :: !Int
  , dbUser :: !String
  , dbPassword :: !String
  , dbName :: !String
  } deriving (Show)

type DB = P.Pool
type Statement = S.Session
type Result a = Either P.UsageError a

class WithDB m where
  getDB :: m DB

runWithDB :: Options -> (DB -> IO ()) -> IO ()
runWithDB opts = bracket (P.acquire $ poolOpts opts) P.release

run :: (MonadIO m, WithDB m) => Statement a -> m (Result a)
run statement = getDB >>= liftIO . flip P.use statement

poolOpts :: Options -> P.Settings
poolOpts Options{..} = (poolSize, fromIntegral poolTimeout,
  C.settings host port user password name)
  where
    host = fromString dbHost
    port = fromIntegral dbPort
    user = fromString dbUser
    password = fromString dbPassword 
    name = fromString dbName

migrate :: DB -> IO (Result ())
migrate db = P.use db $ do
  migrations <- liftIO $ M.loadMigrationsFromDirectory "./migrations"
  forM_ (M.MigrationInitialization : migrations) $ \m ->
    T.transaction T.Serializable T.Write $ M.runMigration m

