{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Health
  ( healthApi
  ) where

import Control.Monad.Trans
import Data.Int (Int64)
import Hasql.TH as TH
import Network.HTTP.Types.Status
import Web.Scotty.Trans as S

import App.DB as DB
import App.Web as Web

healthApi :: (WithDB m, MonadIO m) => Scotty m ()
healthApi = get "/health" $
  selectLiteral >>= \case
    Right _ -> text "OK"
    Left  e -> liftIO (print e) >> status status503 >> text "Error"

selectLiteral :: (WithDB m, MonadIO m) => Action m (DB.Result Int64)
selectLiteral = lift . DB.run $ statement () [TH.singletonStatement|
  select 1 :: int8
  |]
