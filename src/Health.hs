{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Health
  ( healthApi
  ) where

import Control.Monad.Trans
import Data.Int (Int64)
import Data.Text.Lazy
import Hasql.Session as DB
import Hasql.TH as TH
import Network.HTTP.Types.Status
import Web.Scotty.Trans as S

import Env

healthApi :: (WithPool m, MonadIO m) => ScottyT Text m ()
healthApi = get "/health" $
  usePool selectLiteral >>= \case
    Right _ -> text "OK"
    Left  e -> liftIO (print e) >> status status503 >> text "Error"

selectLiteral :: DB.Session Int64
selectLiteral = DB.statement () [TH.singletonStatement|
  select 1 :: int8
  |]
