{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Todo.Web
  ( todoApi,
    TodoApi,
  )
where

import App.Error (catch, catchLast)
import App.Paging (Page (..))
import App.Web (WebHandler)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, lift)
import Data.Identifier (Identifier (..))
import Data.Maybe (fromMaybe)
import Data.UUID (fromText)
import Servant
import Todo.Domain
  ( CreateTodoRequest,
    Logic,
    MissingFields (..),
    NotExists (..),
    Todo,
    TodoId,
    TodoMaybe,
    create,
    delete,
    identifier,
    modify,
    patch,
    showPage,
  )
import Todo.JSON ()
import Prelude hiding (id)

type TodoApi =
  "todo" :> QueryParam "offset" Word :> QueryParam "limit" Word :> Get '[JSON] [Todo]
    :<|> "todo" :> ReqBody '[JSON] CreateTodoRequest :> PostCreated '[JSON] Todo
    :<|> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] Todo
    :<|> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] TodoMaybe :> Patch '[JSON] Todo
    :<|> "todo" :> Capture "id" TodoId :> Delete '[JSON] ()

todoApi :: (MonadIO m, Logic m) => ServerT TodoApi (WebHandler m)
todoApi =
  listHandler :<|> createHandler :<|> modifyHandler :<|> patchHandler :<|> deleteHandler
  where
    listHandler moffset mlimit = do
      let offset = fromMaybe 0 moffset
          limit = fromMaybe 20 mlimit
      lift $ showPage $ Page offset limit

    createHandler todo = do
      lift $ create todo

    modifyHandler id todo = do
      when (id /= identifier todo) identifierError
      lift (modify todo) >>= handleModifyError
    patchHandler id todo = do
      when (id /= identifier todo) identifierError
      lift (patch todo) >>= handlePatchError

    deleteHandler id =
      lift (delete id) >>= handleDeleteError

    identifierError = throwError $ err400 {errBody = "Identifiers in path and body are different"}
    notExistsError = throwError $ err400 {errBody = "Todo with provided identifier has not been found"}
    missingFieldsError = throwError $ err400 {errBody = "Could not construct final Todo record"}

    handlePatchError result =
      fmap return result
        `catch` \case MissingFields -> return missingFieldsError
        `catchLast` \case NotExists -> notExistsError

    handleModifyError result =
      fmap return result
        `catchLast` \case NotExists -> notExistsError

    handleDeleteError = handleModifyError

instance FromHttpApiData TodoId where
  parseUrlPiece text = maybe (Left "Could not read TodoId") (Right . Identifier) $ fromText text
