{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Health
  ( healthApi,
    HealthApi,
  )
where

import App.DB as DB (WithDB, execute, statement)
import App.Web (WebHandler)
import Control.Monad.Trans (MonadIO, lift)
import Data.ByteString.Lazy.Char8
import Data.Int (Int64)
import Data.Swagger
import GHC.Generics
import qualified Hasql.Decoders as D (column, int8, nonNullable, singleRow)
import qualified Hasql.Encoders as E (noParams)
import Servant

newtype HealthIndicator = HealthIndicator String
  deriving (Generic)

instance ToSchema HealthIndicator

type HealthApi = Summary "Check application health" :> "health" :> Get '[PlainText] HealthIndicator

instance MimeRender PlainText HealthIndicator where
  mimeRender _ (HealthIndicator i) = pack i

healthApi :: (WithDB m, MonadIO m) => ServerT HealthApi (WebHandler m)
healthApi =
  lift selectLiteral >> return (HealthIndicator "OK")

selectLiteral :: (WithDB m, MonadIO m) => m Int64
selectLiteral = DB.execute $ statement "select 1" E.noParams decoder ()
  where
    decoder = D.singleRow $ D.column $ D.nonNullable D.int8
