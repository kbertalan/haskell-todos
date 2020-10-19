module Todo
  ( Todo
  , todoApi
  ) where

import Control.Monad.Reader
import Data.Text.Lazy
import Data.UUID
import Web.Scotty.Trans

import Env

data Todo = Todo
  { description :: !Text
  , uid :: UUID
  , completed :: !Bool
  } deriving (Show)

todoApi :: (MonadReader Env m, MonadIO m) => ScottyT Text m ()
todoApi = get "/todo" $ raise "OK"

