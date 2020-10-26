{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module App.Web.Error
  ( errorHandler
  ) where

import Data.Aeson (ToJSON)
import Data.Text.Lazy
import GHC.Generics (Generic)
import Network.HTTP.Types.Status
import Web.Scotty.Trans as S

errorHandler :: (Monad m) => ScottyT Text m ()
errorHandler =
  defaultHandler $ \msg -> do
    status status500
    S.json $ Error msg

newtype Error = Error
  { message :: Text
  } deriving stock (Generic)
  deriving anyclass (ToJSON)

