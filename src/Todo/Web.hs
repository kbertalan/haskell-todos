module Todo.Web
  ( todoApi
  ) where

import Control.Monad.Trans
import Network.HTTP.Types.Status
import Web.Scotty.Trans          as W

import App.Web
import Todo.Domain

todoApi :: (MonadIO m, Logic m) => Scotty m ()
todoApi = do
  get "/todo" $
    lift showAll >>= json
  post "/todo" $
    jsonData >>= lift . create >>= \r -> do
      status status201
      json r
  put "/todo" $
    jsonData >>= lift . modify >>= handleModifyError >>= json
  W.patch "/todo" $
    jsonData >>= lift . Todo.Domain.patch >>= handlePatchError >>= json

handleModifyError :: Monad m => Either ModifyError a -> Action m a
handleModifyError result =
  case result of
    Right r              -> return r
    Left ModifyNotExists -> raiseStatus status404 "Todo with provided identifier has not been found"

handlePatchError :: Monad m => Either PatchError a -> Action m a
handlePatchError result =
  case result of
    Right r             -> return r
    Left MissingId      -> raiseStatus status400 "No identifier has been provided"
    Left MissingFields  -> raiseStatus status400 "Could not construct final Todo record"
    Left PatchNotExists -> raiseStatus status404 "Todo with provided identifier has not been found"

