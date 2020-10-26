module App.Web
  ( Options(..)
  , Scotty
  , Action
  , run
  ) where

import Control.Monad.IO.Class
import Data.Text.Lazy (Text)
import Network.Wai (Response)
import qualified Web.Scotty.Trans as S

import App.Web.Error (errorHandler)

newtype Options = Options
  { webPort :: Int
  } deriving (Show)

type Scotty = S.ScottyT Text
type Action = S.ActionT Text

run :: (Monad m, MonadIO n) => Options -> (m Response -> IO Response) -> Scotty m () -> n ()
run opts runner routes = S.scottyT (webPort opts) runner $ do
  errorHandler
  routes

