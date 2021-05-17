{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Health
  ( healthApi,
    HealthApi,
  )
where

import App.DB as DB (Pool, execute, runWithConnection, statement)
import App.Web (WebHandler)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans (lift)
import Data.ByteString.Lazy.Char8
import Data.Has (Has)
import Data.Int (Int64)
import Data.Swagger
import GHC.Generics
import qualified Hasql.Decoders as D (column, int8, nonNullable, singleRow)
import qualified Hasql.Encoders as E (noParams)
import Servant
import UnliftIO (MonadUnliftIO)

newtype HealthIndicator = HealthIndicator String
  deriving (Generic)

instance ToSchema HealthIndicator

type HealthApi = Summary "Check application health" :> "health" :> Get '[PlainText] HealthIndicator

instance MimeRender PlainText HealthIndicator where
  mimeRender _ (HealthIndicator i) = pack i

healthApi :: (MonadReader env m, Has Pool env, MonadUnliftIO m) => ServerT HealthApi (WebHandler m)
healthApi =
  lift selectLiteral >> return (HealthIndicator "OK")

selectLiteral :: (MonadReader env m, Has Pool env, MonadUnliftIO m) => m Int64
selectLiteral = DB.runWithConnection $ runReaderT $ DB.execute $ statement "select 1" E.noParams decoder ()
  where
    decoder = D.singleRow $ D.column $ D.nonNullable D.int8
