{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.DB
  ( Options (..),
    WithPool,
    getPool,
    Pool,
    runWithPool,
    execute,
    statement,
    migrate,
  )
where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.List (sortOn)
import qualified Hasql.Connection as C (settings)
import qualified Hasql.Decoders as D (Result)
import qualified Hasql.Encoders as E (Params)
import qualified Hasql.Migration as M (MigrationCommand (..), runMigration)
import qualified Hasql.Pool as P (Pool, Settings, UsageError, acquire, release, use)
import qualified Hasql.Session as S (Session, statement)
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

type Pool = P.Pool

newtype DBException = DBException P.UsageError
  deriving (Show)
  deriving anyclass (Exception)

class WithPool m where
  getPool :: m Pool

runWithPool :: Options -> (Pool -> IO ()) -> IO ()
runWithPool opts = bracket (P.acquire $ poolOpts opts) P.release

execute :: (MonadIO m, WithPool m) => S.Session a -> m a
execute session =
  getPool >>= liftIO . flip P.use session >>= \case
    Left e -> liftIO . throwIO $ DBException e
    Right r -> return r

poolOpts :: Options -> P.Settings
poolOpts Options {..} =
  ( poolSize,
    fromIntegral poolTimeout,
    C.settings dbHost (fromIntegral dbPort) dbUser dbPassword dbName
  )

migrate :: Pool -> IO (Either P.UsageError ())
migrate db = P.use db $ do
  let migrations = uncurry M.MigrationScript <$> sortOn fst embeddedMigrations
  forM_ (M.MigrationInitialization : migrations) $ \m -> do
    T.transaction T.Serializable T.Write $ M.runMigration m

embeddedMigrations :: [(FilePath, ByteString)]
embeddedMigrations = $(embedDir "./migrations")

statement :: ByteString -> E.Params a -> D.Result b -> a -> S.Session b
statement sql paramsEncoder resultDecoder =
  flip S.statement $ St.Statement sql paramsEncoder resultDecoder True
