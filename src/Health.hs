{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Health
  ( healthApi,
    HealthApi,
  )
where

import App.DB as DB (WithDB, execute, statement)
import App.Web (WebHandler)
import Control.Monad.Trans (MonadIO, lift)
import Data.Int (Int64)
import qualified Hasql.Decoders as D (column, int8, nonNullable, singleRow)
import qualified Hasql.Encoders as E (noParams)
import Servant

type HealthApi = "health" :> Get '[PlainText] String

healthApi :: (WithDB m, MonadIO m) => ServerT HealthApi (WebHandler m)
healthApi =
  lift selectLiteral >> return "OK"

selectLiteral :: (WithDB m, MonadIO m) => m Int64
selectLiteral = DB.execute $ statement "select 1" E.noParams decoder ()
  where
    decoder = D.singleRow $ D.column $ D.nonNullable D.int8
