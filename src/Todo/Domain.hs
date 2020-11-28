{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Todo.Domain
  ( Todo(..)
  , TodoM(..)
  , TodoMaybe
  , TodoLast
  , fromTodo
  , toTodo
  , CreateTodoRequest(..)
  , Logic
  , showAll
  , create
  , modify
  , patch
  , delete
  , ModifyError(..)
  , PatchError (..)
  , DeleteError (..)
  , repoGetById
  , repoUpdate
  , repoSelectAll
  , repoInsert
  , repoDelete
  , logicCreate
  , logicUpdate
  , logicPatch
  , logicDelete
  , Repo
  ) where

import App.Error            (throwIfNothing)
import Control.Monad        (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Random (MonadRandom, getRandom)
import Data.Aeson           (FromJSON, ToJSON, parseJSON, withObject, (.:), (.:?))
import Data.Coerce          (coerce)
import Data.Monoid          (Last (..), getLast)
import Data.Text.Lazy       (Text)
import Data.UUID            (UUID)
import GHC.Generics         (Generic)
import Prelude              hiding (id)

data Todo = Todo
  { id          :: !UUID
  , description :: !Text
  , completed   :: !Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

data TodoM m = TodoM
  { mId          :: m UUID
  , mDescription :: m Text
  , mCompleted   :: m Bool
  }

type TodoLast = TodoM Last
type TodoMaybe = TodoM Maybe

instance Semigroup (TodoM Last) where
  TodoM i1 d1 c1 <> TodoM i2 d2 c2 = TodoM (i1 <> i2) (d1 <> d2) (c1 <> c2)

fromTodo :: (forall a. a -> m a) -> Todo -> TodoM m
fromTodo f Todo{..} = TodoM
  { mId = f id
  , mDescription = f description
  , mCompleted = f completed
  }

toTodo :: (forall a. m a -> Maybe a) -> TodoM m -> Maybe Todo
toTodo f TodoM{..} = Todo
  <$> f mId
  <*> f mDescription
  <*> f mCompleted

instance FromJSON (TodoM Maybe) where
  parseJSON = withObject "Todo" $ \v -> TodoM
    <$> v .:? "id"
    <*> v .:? "description"
    <*> v .:? "completed"

newtype CreateTodoRequest = CreateTodoRequest
  { ctrDescription :: Text
  } deriving (Show, Generic)

instance FromJSON CreateTodoRequest where
  parseJSON = withObject "CreateTodoRequest" $ \v -> CreateTodoRequest
    <$> v .: "description"

data ModifyError
  = ModifyNotExists
  | ModifyIdentifierMismatch
  deriving (Show)

data PatchError
  = MissingId
  | MissingFields
  | PatchNotExists
  | PatchIdentifierMismatch
  deriving Show

data DeleteError
  = DeleteNotExists
  deriving Show

class Logic m where
  showAll :: m [Todo]
  create :: CreateTodoRequest -> m Todo
  modify :: UUID -> Todo -> m (Either ModifyError Todo)
  patch :: UUID -> TodoMaybe -> m (Either PatchError Todo)
  delete :: UUID -> m (Either DeleteError ())

class Repo m where
  repoSelectAll :: m [Todo]
  repoInsert :: Todo -> m Todo
  repoUpdate :: Todo -> m Todo
  repoGetById :: UUID -> m (Maybe Todo)
  repoDelete :: UUID -> m ()

logicCreate :: (Repo m, MonadRandom m) => CreateTodoRequest -> m Todo
logicCreate req = do
  newId <- getRandom
  let todo = Todo {
      Todo.Domain.id = newId
    , description = ctrDescription req
    , completed = False
    }
  repoInsert todo

logicUpdate :: (Repo m, MonadError ModifyError m) => UUID -> Todo -> m Todo
logicUpdate identifier todo = do
  when (identifier /= Todo.Domain.id todo) $ throwError ModifyIdentifierMismatch
  repoGetById (Todo.Domain.id todo)
    >>= throwIfNothing ModifyNotExists
    >> repoUpdate todo

logicPatch :: (Repo m, MonadError PatchError m) => UUID -> TodoMaybe -> m Todo
logicPatch identifier req = do
  existingId <- throwIfNothing MissingId $ mId req
  when (identifier /= existingId) $ throwError PatchIdentifierMismatch
  existing <- repoGetById existingId >>= throwIfNothing PatchNotExists
  let existingLast = fromTodo (Last . Just) existing
  todo <- throwIfNothing MissingFields $ toTodo getLast $ existingLast <> coerce req
  repoUpdate todo

logicDelete :: (Repo m, MonadError DeleteError m) => UUID -> m ()
logicDelete identifier = do
  _existing <- repoGetById identifier >>= throwIfNothing DeleteNotExists
  repoDelete identifier

