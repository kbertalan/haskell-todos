{-# OPTIONS_GHC -Wno-orphans #-}

module Todo.Web
  ( todoApi,
  )
where

import App.Error (catch, catchLast)
import App.Paging (Page (..))
import App.Web (Scotty, jsonError)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, lift)
import Data.Text.Lazy (unpack)
import Data.UUID (UUID)
import Network.HTTP.Types.Status (status201, status400, status404)
import Text.Read (readMaybe)
import Todo.Domain
  ( Identifier (..),
    Logic,
    MissingFields (..),
    MissingId (..),
    NotExists (..),
    create,
    delete,
    identifier,
    modify,
    patch,
    showPage,
  )
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

  post "/todo" $ do
    jsonData >>= lift . create >>= \r -> do
      status status201
      json r

  put "/todo/:id" $ do
    idValue <- Identifier <$> param "id"
    todo <- jsonData
    when (idValue /= identifier todo) identifierError
    lift (modify todo) >>= handleModifyError >>= json

  W.patch "/todo/:id" $ do
    idValue <- Identifier <$> param "id"
    todo <- jsonData
    when (Just idValue /= identifier todo) identifierError
    lift (Todo.Domain.patch todo) >>= handlePatchError >>= json

  W.delete "/todo/:id" $
    param "id" >>= lift . Todo.Domain.delete . Identifier >>= handleDeleteError >>= json
  where
    identifierError = jsonError status400 "Identifiers in path and body are different"
    notExistsError = jsonError status404 "Todo with provided identifier has not been found"
    missingIdError = jsonError status400 "No identifier has been found in request"
    missingFieldsError = jsonError status400 "Could not construct final Todo record"

    handlePatchError result =
      fmap return result
        `catch` \case MissingId -> return missingIdError
        `catch` \case MissingFields -> return missingFieldsError
        `catchLast` \case NotExists -> notExistsError

    handleModifyError result =
      fmap return result
        `catchLast` \case NotExists -> notExistsError

    handleDeleteError = handleModifyError

instance Parsable UUID where
  parseParam = maybe (Left "Invalid UUID") Right . readMaybe . unpack
