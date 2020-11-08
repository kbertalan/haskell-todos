module Todo.Web
  ( todoApi
  ) where

import Control.Monad.Trans       (MonadIO, lift)
import Network.HTTP.Types.Status (status201, status400, status404)
import Web.Scotty.Trans          as W (get, json, jsonData, patch, post, put, raiseStatus, status)

import App.Web                   (Action, Scotty, jsonError)
import Todo.Domain               (Logic, ModifyError (..), PatchError (..), create, modify, patch, showAll)

todoApi :: (MonadIO m, Logic m) => Scotty m ()
todoApi = do
  get "/todo" $
    lift showAll >>= json
  post "/todo" $ do
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
    Left MissingId      -> jsonError status400 "No identifier has been provided"
    Left MissingFields  -> jsonError status400 "Could not construct final Todo record"
    Left PatchNotExists -> jsonError status404 "Todo with provided identifier has not been found"

