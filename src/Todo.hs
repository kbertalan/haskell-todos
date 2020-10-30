{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards #-}

module Todo
  ( Todo
  , todoApi
  , TodoActions
  , showAll
  , createNew
  , TodoRepo
  , Todo.all
  , add
  -- implementations
  , createNewAction
  -- repo implementations
  , selectAllTodos
  , insertTodo
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, object, withObject, (.=), (.:))
import Data.Bifunctor (first)
import Data.Functor.Contravariant ((>$<))
import Data.Text as T
import Data.Text.Lazy as L
import Data.UUID
import GHC.Generics (Generic)
import Hasql.Statement
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Network.HTTP.Types.Status
import Web.Scotty.Trans

import App.DB as DB
import App.Web as Web

data Todo = Todo
  { uid :: UUID
  , description :: !T.Text
  , completed :: !Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype CreateTodoRequest = CreateTodoRequest
  { ctrDescription :: T.Text
  } deriving (Show, Generic)

instance ToJSON CreateTodoRequest where
  toJSON CreateTodoRequest {..} = object
    [ "description" .= ctrDescription ]

instance FromJSON CreateTodoRequest where
  parseJSON = withObject "CreateTodoRequest" $ \v -> CreateTodoRequest
    <$> v .: "description"

todoApi :: (MonadIO m, TodoActions m) => Scotty m ()
todoApi = do
  get "/todo" $
    lift showAll >>= \case
      Right r -> json r
      Left  e -> raise $ L.pack $ show e
  post "/todo" $
    jsonData >>= lift . createNew >>= \case
      Right r -> do
        status status201
        json r
      Left  e -> raise $ L.pack $ show e

newtype Error = Error L.Text
  deriving (Show)

type Result a = Either Error a

class TodoActions m where
  showAll :: m (Todo.Result [Todo])
  createNew :: CreateTodoRequest -> m (Todo.Result Todo)

class TodoRepo m where
  all :: m (Todo.Result [Todo])
  add :: Todo -> m (Todo.Result Todo)

createNewAction :: (TodoRepo m) => CreateTodoRequest -> m (Todo.Result Todo)
createNewAction req =
  let todo = Todo { uid = nil -- TODO generate random id here and remove postgresql random generation
    , description = ctrDescription req
    , completed = False
    }
  in
    add todo

selectAllTodos :: (MonadIO m, WithDB m) => m (Todo.Result [Todo])
selectAllTodos = convertError $ DB.run $ statement () $
  Statement
    "select id, description, completed from todo order by created_at asc"
    E.noParams
    decoder
    True
  where
    decoder = D.rowList row

insertTodo :: (MonadIO m, WithDB m) => Todo -> m (Todo.Result Todo)
insertTodo todo = convertError $ DB.run $ statement todo $
  Statement
    "insert into todo (id, description, completed, created_at, last_updated_at)\
    \ values (uuid_generate_v4(), $1, $2, now(), now())\
    \ returning id, description, completed"
    encoder
    decoder
    True
  where
    encoder =
      (description >$< E.param (E.nonNullable E.text))
      <> (completed >$< E.param (E.nonNullable E.bool))
    decoder = D.singleRow row

convertError :: (Monad m) => m (DB.Result a) -> m (Todo.Result a)
convertError = fmap (first $ Error . L.pack . show)

row :: D.Row Todo
row = Todo
  <$> D.column (D.nonNullable D.uuid)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.bool)
