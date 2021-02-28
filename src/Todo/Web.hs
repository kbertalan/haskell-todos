{-# OPTIONS_GHC -Wno-orphans #-}

module Todo.Web
  ( todoApi,
  )
where

import App.Error (catch, catchLast)
import App.Paging (Page (..))
import App.Web (Action, Scotty, jsonError)
import Control.Monad (when)
import Control.Monad.Identity (Identity)
import Control.Monad.Trans (MonadIO, lift)
import Data.Aeson (FromJSON)
import Data.Text.Lazy (unpack)
import Data.UUID (UUID)
import Network.HTTP.Types.Status (status201, status400, status404)
import Text.Read (readMaybe)
import Todo.Domain
  ( Identifier (..),
    Logic,
    MissingFields (..),
    NotExists (..),
    TodoM,
    create,
    delete,
    identifier,
    modify,
    patch,
    showPage,
  )
import Todo.JSON ()
import Web.Scotty.Trans as W
  ( Parsable,
    delete,
    get,
    json,
    jsonData,
    param,
    parseParam,
    patch,
    post,
    put,
    rescue,
    status,
  )

todoApi :: (MonadIO m, Logic m) => Scotty m ()
todoApi = do
  get "/todo" $ do
    offset <- param "offset" `rescue` const (return 0)
    limit <- param "limit" `rescue` const (return 20)
    lift (showPage (Page offset limit)) >>= json

  post "/todo" $
    jsonData >>= lift . create >>= \r ->
      status status201 >> json r

  put "/todo/:id" $
    obtainTodo >>= lift . modify >>= handleModifyError >>= json

  W.patch "/todo/:id" $
    obtainTodo >>= lift . Todo.Domain.patch >>= handlePatchError >>= json

  W.delete "/todo/:id" $
    param "id" >>= lift . Todo.Domain.delete . Identifier >>= handleDeleteError >>= json
  where
    obtainTodo :: (MonadIO m, FromJSON (TodoM Identity n)) => Action m (TodoM Identity n)
    obtainTodo = do
      idValue <- Identifier <$> param "id"
      todo <- jsonData
      when (idValue /= identifier todo) identifierError
      return todo

    identifierError :: (MonadIO m) => Action m a
    identifierError = jsonError status400 "Identifiers in path and body are different"
    notExistsError = jsonError status404 "Todo with provided identifier has not been found"
    missingFieldsError = jsonError status400 "Could not construct final Todo record"

    handlePatchError result =
      fmap return result
        `catch` \case MissingFields -> return missingFieldsError
        `catchLast` \case NotExists -> notExistsError

    handleModifyError result =
      fmap return result
        `catchLast` \case NotExists -> notExistsError

    handleDeleteError = handleModifyError

instance Parsable UUID where
  parseParam = maybe (Left "Invalid UUID") Right . readMaybe . unpack
