{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Todo.Domain
  ( Identifier (..),
    Entity (..),
    TodoId,
    TodoF (..),
    Todo,
    TodoMaybe,
    TodoLast,
    CreateTodoRequest (..),
    Logic,
    showPage,
    create,
    modify,
    patch,
    delete,
    ModifyError,
    NotExists (..),
    MissingFields (..),
    PatchError,
    DeleteError,
    repoGetById,
    repoUpdate,
    repoSelectPage,
    repoInsert,
    repoDelete,
    logicCreate,
    logicUpdate,
    logicPatch,
    logicDelete,
    Repo,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.Identity (Identity)
import Control.Monad.Random (MonadRandom, getRandom)
import Data.Error (OneOf, throwIfNothing)
import Data.Field (Field)
import Data.Function ((&))
import Data.Identifier (Identifier (..))
import Data.Monoid (Last (..), getLast)
import Data.Paging (Page)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

type TodoId = Identifier (TodoF Identity)

data Entity i a = Entity
  { identifier :: i,
    record :: a
  }
  deriving (Generic, Show, Eq)

data TodoF f = TodoF
  { description :: Field f Text,
    completed :: Field f Bool
  }
  deriving (Generic)

type Todo = Entity TodoId (TodoF Identity)

type TodoLast = TodoF Last

type TodoMaybe = Entity TodoId (TodoF Maybe)

deriving instance Eq (TodoF Identity)

deriving instance Show (TodoF Identity)

deriving instance Eq (TodoF Maybe)

deriving instance Show (TodoF Maybe)

instance Semigroup TodoLast where
  TodoF d1 c1 <> TodoF d2 c2 = TodoF (d1 <> d2) (c1 <> c2)

newtype CreateTodoRequest = CreateTodoRequest
  { ctrDescription :: Text
  }
  deriving (Eq, Show, Generic)

data NotExists = NotExists
  deriving (Show, Eq)

data MissingFields = MissingFields
  deriving (Show, Eq)

type ModifyError = NotExists

type PatchError e = OneOf e '[MissingFields, NotExists]

type DeleteError = NotExists

class Logic m where
  showPage :: Page -> m [Todo]
  create :: CreateTodoRequest -> m Todo
  modify :: Todo -> m (Either ModifyError Todo)
  patch :: (PatchError e) => TodoMaybe -> m (Either e Todo)
  delete :: TodoId -> m (Either DeleteError ())

class Repo m where
  repoSelectPage :: Page -> m [Todo]
  repoInsert :: Todo -> m Todo
  repoUpdate :: Todo -> m Todo
  repoGetById :: TodoId -> m (Maybe Todo)
  repoDelete :: TodoId -> m ()

logicCreate :: (Repo m, MonadRandom m) => CreateTodoRequest -> m Todo
logicCreate req = do
  newId <- getRandom
  let todo =
        Entity
          { identifier = Identifier newId,
            record =
              TodoF
                { description = ctrDescription req,
                  completed = False
                }
          }
  repoInsert todo

logicUpdate :: (Repo m, MonadError ModifyError m) => Todo -> m Todo
logicUpdate todo = do
  repoGetById (identifier todo)
    >>= throwIfNothing NotExists
    >> repoUpdate todo

logicPatch :: (Repo m, MonadError e m, PatchError e) => TodoMaybe -> m Todo
logicPatch req = do
  let existingId = identifier req
  existing <- repoGetById existingId >>= throwIfNothing NotExists
  let existingLast = convertTodoF @Identity @Last (Last . Just) $ record existing
      reqLast = convertTodoF @Maybe @Last Last $ record req
  todoRecord <-
    existingLast <> reqLast
      & toTodo getLast
      & throwIfNothing MissingFields
  repoUpdate $ Entity existingId todoRecord

logicDelete :: (Repo m, MonadError DeleteError m) => TodoId -> m ()
logicDelete i = do
  _existing <- repoGetById i >>= throwIfNothing NotExists
  repoDelete i

toTodo :: (Applicative g) => (forall a. Field f a -> g a) -> TodoF f -> g (TodoF Identity)
toTodo f TodoF {..} =
  TodoF
    <$> f description
    <*> f completed

convertTodoF :: forall f g. (forall a. Field f a -> Field g a) -> TodoF f -> TodoF g
convertTodoF f TodoF {..} =
  TodoF
    { description = f @Text description,
      completed = f @Bool completed
    }
