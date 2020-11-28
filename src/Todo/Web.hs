{-# OPTIONS_GHC -Wno-orphans #-}

module Todo.Web
  ( todoApi
  ) where

import Control.Monad.Trans       (MonadIO, lift)
import Data.Text.Lazy            (unpack)
import Data.UUID                 (UUID)
import Network.HTTP.Types.Status (status201, status400, status404)
import Text.Read                 (readMaybe)
import Web.Scotty.Trans          as W (Parsable, delete, get, json, jsonData, param, parseParam, patch, post, put,
                                       status)

import App.Web                   (Action, Scotty, jsonError)
import Todo.Domain               (DeleteError (..), Logic, ModifyError (..), PatchError (..), create, delete, modify,
                                  patch, showAll)

todoApi :: (MonadIO m, Logic m) => Scotty m ()
todoApi = do
  get "/todo" $
    lift showAll >>= json
  post "/todo" $ do
    jsonData >>= lift . create >>= \r -> do
      status status201
      json r
  put "/todo/:id" $ do
    identifier <- param "id"
    jsonData >>= lift . modify identifier >>= handleModifyError >>= json
  W.patch "/todo/:id" $ do
    identifier <- param "id"
    jsonData >>= lift . Todo.Domain.patch identifier >>= handlePatchError >>= json
  W.delete "/todo/:id" $
    param "id" >>= lift . Todo.Domain.delete >>= handleDeleteError >>= json

handleModifyError :: Monad m => Either ModifyError a -> Action m a
handleModifyError result =
  case result of
    Right r                       -> return r
    Left ModifyNotExists          -> jsonError status404 "Todo with provided identifier has not been found"
    Left ModifyIdentifierMismatch -> jsonError status400 "Identifiers in path and body are different"

handlePatchError :: Monad m => Either PatchError a -> Action m a
handlePatchError result =
  case result of
    Right r                      -> return r
    Left MissingId               -> jsonError status400 "No identifier has been provided"
    Left MissingFields           -> jsonError status400 "Could not construct final Todo record"
    Left PatchNotExists          -> jsonError status404 "Todo with provided identifier has not been found"
    Left PatchIdentifierMismatch -> jsonError status400 "Identifiers in path and body are different"

handleDeleteError :: Monad m => Either DeleteError a -> Action m a
handleDeleteError result =
  case result of
    Right r              -> return r
    Left DeleteNotExists -> jsonError status404 "Todo with provided identifier has not been found"

instance Parsable UUID where
  parseParam = maybe (Left "Invalid UUID") Right . readMaybe . unpack

