{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module App.Web
  ( Options(..)
  , Scotty
  , Action
  , run
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson                (ToJSON)
import           Data.Text.Lazy            (Text)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types.Status
import           Network.Wai               (Response)
import qualified Web.Scotty.Trans          as S

newtype Options = Options
  { webPort :: Int
  } deriving (Show)

type Scotty = S.ScottyT Text
type Action = S.ActionT Text

run :: (Monad m, MonadIO n) => Options -> (m Response -> IO Response) -> Scotty m () -> n ()
run opts runner routes = S.scottyT (webPort opts) runner $ do
  errorHandler
  routes

errorHandler :: (Monad m) => Scotty m ()
errorHandler =
  S.defaultHandler $ \msg -> do
    S.status status500
    S.json $ Error msg

newtype Error = Error
  { message :: Text
  } deriving stock (Generic)
  deriving anyclass (ToJSON)
