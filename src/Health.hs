module Health
  ( healthApi
  ) where

import           Control.Monad.Trans (MonadIO, lift)
import           Data.Int            (Int64)
import qualified Hasql.Decoders      as D (column, int8, nonNullable, singleRow)
import qualified Hasql.Encoders      as E (noParams)
import           Web.Scotty.Trans    (get, text)

import           App.DB              as DB (WithDB, execute, statement)
import           App.Web             (Scotty)

healthApi :: (WithDB m, MonadIO m) => Scotty m ()
healthApi = get "/health" $
  lift selectLiteral >> text "OK"

selectLiteral :: (WithDB m, MonadIO m) => m Int64
selectLiteral = DB.execute $ statement "select 1" E.noParams decoder ()
  where
    decoder = D.singleRow $ D.column $ D.nonNullable D.int8

