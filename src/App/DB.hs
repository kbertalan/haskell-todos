{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.DB
  ( Options (..),
    WithPool (..),
    Pool,
    runWithPool,
    execute,
    statement,
    migrate,
    WithConnection (..),
    Connection,
  )
where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.List (sortOn)
import qualified Data.Pool as P
import qualified Hasql.Connection as C
import qualified Hasql.Decoders as D (Result)
import qualified Hasql.Encoders as E (Params)
import qualified Hasql.Migration as M (MigrationCommand (..), runMigration)
import qualified Hasql.Session as S (QueryError, Session, run, statement)
import qualified Hasql.Statement as St (Statement (..))
import qualified Hasql.Transaction.Sessions as T (IsolationLevel (Serializable), Mode (Write), transaction)

data Options = Options
  { poolSize :: !Int,
    poolTimeout :: !Int,
    dbHost :: !ByteString,
    dbPort :: !Int,
    dbUser :: !ByteString,
    dbPassword :: !ByteString,
    dbName :: !ByteString
  }
  deriving (Show)

type Pool = P.Pool C.Connection

type Connection = C.Connection

data DBException
  = ConnectionException C.ConnectionError
  | StatementException S.QueryError
  deriving (Show)
  deriving anyclass (Exception)

class WithPool m where
  getPool :: m Pool

class WithConnection m where
  getConnection :: m Connection

runWithPool :: Options -> (Pool -> IO ()) -> IO ()
runWithPool Options {..} = bracket acquire release
  where
    acquire =
      P.createPool
        createConnection
        C.release
        1
        (fromIntegral poolTimeout)
        poolSize
    release = P.destroyAllResources
    createConnection =
      C.acquire settings >>= \case
        Right r -> pure r
        Left e -> throwIO $ ConnectionException e
    settings = C.settings dbHost (fromIntegral dbPort) dbUser dbPassword dbName

execute :: (MonadIO m, WithPool m) => S.Session a -> m a
execute session = getPool >>= liftIO . flip usePool session

usePool :: Pool -> S.Session a -> IO a
usePool pool session =
  P.withResource pool (S.run session) >>= \case
    Left e -> throwIO $ StatementException e
    Right r -> pure r

migrate :: Pool -> IO ()
migrate pool = usePool pool $ do
  let migrations = uncurry M.MigrationScript <$> sortOn fst embeddedMigrations
  forM_ (M.MigrationInitialization : migrations) $ \m -> do
    T.transaction T.Serializable T.Write $ M.runMigration m

embeddedMigrations :: [(FilePath, ByteString)]
embeddedMigrations = $(embedDir "./migrations")

statement :: ByteString -> E.Params a -> D.Result b -> a -> S.Session b
statement sql paramsEncoder resultDecoder =
  flip S.statement $ St.Statement sql paramsEncoder resultDecoder True
