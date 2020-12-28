{-# OPTIONS_GHC -Wno-orphans #-}

module Todo.Web
  ( todoApi
  ) where

import Control.Monad             (when)
import Control.Monad.Trans       (MonadIO, lift)
import Data.Text.Lazy            (unpack)
import Data.UUID                 (UUID)
import Network.HTTP.Types.Status (status201, status400, status404)
import Text.Read                 (readMaybe)
import Web.Scotty.Trans          as W (Parsable, delete, get, json, jsonData, param, parseParam, patch, post, put,
                                       rescue, status)

import App.Paging                (Page (..))
import App.Web                   (Action, Scotty, jsonError)
import Todo.Domain               (DeleteError, Logic, ModifyError, NotExists (..), PatchError (..), create, delete,
                                  identifier, modify, patch, showPage)

todoApi :: (MonadIO m, Logic m) => Scotty m ()
todoApi = do
  get "/todo" $ do
    offset <- param "offset" `rescue` const (return 0)
    limit <- param "limit" `rescue` const (return 20)
    lift (showPage (Page offset limit)) >>= json
  post "/todo" $ do
    jsonData >>= lift . create >>= \r -> do
      status status201
      json r
  put "/todo/:id" $ do
    idValue <- param "id"
    todo <- jsonData
    when (idValue /= identifier todo) $ jsonError status400 "Identifiers in path and body are different"
    lift (modify todo) >>= handleModifyError >>= json
  W.patch "/todo/:id" $ do
    idValue <- param "id"
    todo <- jsonData
    when (Just idValue /= identifier todo) $ jsonError status400 "Identifiers in path and body are different"
    lift (Todo.Domain.patch todo) >>= handlePatchError >>= json
  W.delete "/todo/:id" $
    param "id" >>= lift . Todo.Domain.delete >>= handleDeleteError >>= json

handleModifyError :: Monad m => Either ModifyError a -> Action m a
handleModifyError result =
  case result of
    Right r        -> return r
    Left NotExists -> jsonError status404 "Todo with provided identifier has not been found"

handlePatchError :: Monad m => Either PatchError a -> Action m a
handlePatchError result =
  case result of
    Right r             -> return r
    Left MissingId      -> jsonError status400 "No identifier has been provided"
    Left MissingFields  -> jsonError status400 "Could not construct final Todo record"
    Left PatchNotExists -> jsonError status404 "Todo with provided identifier has not been found"

handleDeleteError :: Monad m => Either DeleteError a -> Action m a
handleDeleteError result =
  case result of
    Right r        -> return r
    Left NotExists -> jsonError status404 "Todo with provided identifier has not been found"

instance Parsable UUID where
  parseParam = maybe (Left "Invalid UUID") Right . readMaybe . unpack

