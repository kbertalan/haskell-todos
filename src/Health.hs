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

healthApi :: (UsePool m, MonadIO m) => Scotty m ()
healthApi = get "/health" $
  usePool selectLiteral >>= \case
    Right _ -> text "OK"
    Left  e -> liftIO (print e) >> status status503 >> text "Error"

selectLiteral :: Session Int64
selectLiteral = statement () [TH.singletonStatement|
  select 1 :: int8
  |]
