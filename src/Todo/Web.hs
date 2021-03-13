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
import Data.Maybe (fromJust, fromMaybe)
import Data.UUID (fromText)
import Servant
import Servant.Docs
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
  "todo" :> QueryParamOffset :> QueryParamLimit :> Get '[JSON] [Todo]
    :<|> "todo" :> ReqBody '[JSON] CreateTodoRequest :> PostCreated '[JSON] Todo
    :<|> "todo" :> CaptureTodoId :> ReqBody '[JSON] Todo :> Put '[JSON] Todo
    :<|> "todo" :> CaptureTodoId :> ReqBody '[JSON] TodoMaybe :> Patch '[JSON] Todo
    :<|> "todo" :> CaptureTodoId :> DeleteAccepted '[JSON] NoContent

instance ToParam QueryParamOffset where
  toParam _ = DocQueryParam "offset" ["0", "10", "20"] "offset to query from" Normal

instance ToParam QueryParamLimit where
  toParam _ = DocQueryParam "limit" ["10", "20"] "limit returned query results to this size" Normal

instance ToCapture (Capture "id" TodoId) where
  toCapture _ = DocCapture "id" "Todo identifier"

instance ToSample Todo where
  toSamples _ =
    [ ( "Newly created todo",
        TodoM
          sampleId
          "I will finish this app, I promise"
          False
      )
    ]

instance ToSample TodoMaybe where
  toSamples _ =
    [ ( "Override description",
        TodoM
          sampleId
          (Just "Go shopping")
          Nothing
      )
    ]

instance ToSample CreateTodoRequest where
  toSamples _ =
    [ ("Creating a new todo with only a description", CreateTodoRequest "a new todo")
    ]

sampleId :: TodoId
sampleId = Identifier $ fromJust $ fromText "a990ed3b-a19c-4067-963e-47b26ee0fb43"

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
      lift (delete id) >>= handleDeleteError >> return NoContent

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
