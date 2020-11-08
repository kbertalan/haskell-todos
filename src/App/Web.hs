{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module App.Web
  ( Options(..)
  , Scotty
  , Action
  , run
  , jsonError
  ) where

import Control.Monad.IO.Class    (MonadIO)
import Data.Aeson                (ToJSON)
import Data.Text.Lazy            (Text)
import GHC.Generics              (Generic)
import Network.HTTP.Types.Status (Status)
import Network.Wai               (Response)
import Web.Scotty.Trans          (ActionT, ScottyT, defaultHandler, finish, json, scottyT, status)

newtype Options = Options
  { webPort :: Int
  } deriving (Show)

type Scotty = ScottyT Text
type Action = ActionT Text

run :: (Monad m, MonadIO n) => Options -> (m Response -> IO Response) -> Scotty m () -> n ()
run opts runner routes = scottyT (webPort opts) runner $ do
  errorHandler
  routes

errorHandler :: (Monad m) => Scotty m ()
errorHandler =
  defaultHandler $ \_ -> do
    json $ Error "Something went wrong"

jsonError :: (Monad m) => Status -> Text -> Action m a
jsonError code msg =
  status code >> json (Error msg) >> finish

newtype Error = Error
  { message :: Text
  } deriving stock (Generic)
  deriving anyclass (ToJSON)
