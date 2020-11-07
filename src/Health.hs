module Health
  ( healthApi
  ) where

import           Control.Monad.Trans
import           Data.Int            (Int64)
import qualified Hasql.Decoders      as D
import qualified Hasql.Encoders      as E
import           Hasql.Statement
import           Web.Scotty.Trans    as S

import           App.DB              as DB
import           App.Web             as Web

healthApi :: (WithDB m, MonadIO m) => Scotty m ()
healthApi = get "/health" $
  lift selectLiteral >> S.text "OK"

selectLiteral :: (WithDB m, MonadIO m) => m Int64
selectLiteral = DB.run $ statement () $
  Statement "select 1" E.noParams decoder True
  where
    decoder = D.singleRow $ D.column $ D.nonNullable D.int8

