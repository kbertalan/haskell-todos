{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, LambdaCase #-}

module Health
  ( healthApi
  ) where

import Control.Monad.IO.Class
import Data.Int (Int64)
import Hasql.Pool (use)
import Hasql.Session as DB
import Hasql.TH as TH
import Network.HTTP.Types.Status
import Web.Scotty as S

healthApi pool = get "/health" $ do
  liftIO (use pool selectLiteral) >>= \case
    Right _ -> text "OK"
    Left e -> liftIO (print e) >> status status503 >> text "Error"

selectLiteral :: DB.Session Int64
selectLiteral = DB.statement () [TH.singletonStatement|
  select 1 :: int8
  |]
