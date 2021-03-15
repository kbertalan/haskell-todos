{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Todo.Web
  ( todoApi,
    TodoApi,
  )
where

import App.Error (HandlerOf, catch, catchLast)
import App.Paging (Page (..))
import App.Web (WebHandler)
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Data.Identifier (Identifier (..))
import Data.Maybe (fromMaybe)
import Data.UUID (fromText)
import Servant
import Todo.Domain
  ( CreateTodoRequest (..),
    Logic,
    MissingFields (..),
    NotExists (..),
    Todo,
    TodoId,
    TodoM (..),
    TodoMaybe,
    create,
    delete,
    identifier,
    modify,
    patch,
    showPage,
  )
import Todo.JSON ()
import Todo.Swagger ()
import Prelude hiding (id)

type QueryParamOffset = QueryParam' '[Optional, Strict, Description "return results from this offset"] "offset" Word

type QueryParamLimit = QueryParam' '[Optional, Strict, Description "return maximum limit results"] "limit" Word

type CaptureTodoId = Capture' '[Required, Strict, Description "Todo identifier"] "id" TodoId

type TodoApi =
  Summary "Query Todos" :> "todo" :> QueryParamOffset :> QueryParamLimit :> Get '[JSON] [Todo]
    :<|> Summary "Create new Todo" :> "todo" :> ReqBody '[JSON] CreateTodoRequest :> PostCreated '[JSON] Todo
    :<|> Summary "Update a complete Todo" :> "todo" :> CaptureTodoId :> ReqBody '[JSON] Todo :> Put '[JSON] Todo
    :<|> Summary "Update a Todo using parital data" :> "todo" :> CaptureTodoId :> ReqBody '[JSON] TodoMaybe :> Patch '[JSON] Todo
    :<|> Summary "Delete a Todo" :> "todo" :> CaptureTodoId :> DeleteAccepted '[PlainText] NoContent

todoApi :: (Monad m, Logic m) => ServerT TodoApi (WebHandler m)
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
      lift (delete id) >>= handleDeleteError >> return NoContent

identifierError :: (Monad m) => WebHandler m a
identifierError = throwError $ err400 {errBody = "Identifiers in path and body are different"}

notExistsError :: (Monad m) => WebHandler m a
notExistsError = throwError $ err400 {errBody = "Todo with provided identifier has not been found"}

missingFieldsError :: (Monad m) => WebHandler m a
missingFieldsError = throwError $ err400 {errBody = "Could not construct final Todo record"}

handlePatchError :: (Monad m) => HandlerOf '[MissingFields, NotExists] a -> WebHandler m a
handlePatchError result =
  fmap return result
    `catch` \case MissingFields -> return missingFieldsError
    `catchLast` \case NotExists -> notExistsError

handleModifyError :: (Monad m) => HandlerOf '[NotExists] a -> WebHandler m a
handleModifyError result =
  fmap return result
    `catchLast` \case NotExists -> notExistsError

handleDeleteError :: (Monad m) => HandlerOf '[NotExists] a -> WebHandler m a
handleDeleteError = handleModifyError

instance FromHttpApiData TodoId where
  parseUrlPiece text = maybe (Left "Could not read TodoId") (Right . Identifier) $ fromText text
