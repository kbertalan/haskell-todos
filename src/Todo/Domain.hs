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
    TodoM (..),
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

import App.Error (OneOf, throwIfNothing)
import App.Paging (Page)
import Control.Monad.Except (MonadError)
import Control.Monad.Identity (Identity)
import Control.Monad.Random (MonadRandom, getRandom)
import Data.Field (Field)
import Data.Function ((&))
import Data.Identifier (Identifier (..))
import Data.Monoid (Last (..), getLast)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Prelude hiding (id)

type TodoId = Identifier (TodoM Identity)

data Entity i a = Entity
  { identifier :: i,
    record :: a
  }
  deriving (Generic, Show, Eq)

instance (Arbitrary i, Arbitrary a) => Arbitrary (Entity i a) where
  arbitrary = Entity <$> arbitrary <*> arbitrary

data TodoM m = TodoM
  { description :: Field m Text,
    completed :: Field m Bool
  }
  deriving (Generic)

type Todo = Entity TodoId (TodoM Identity)

type TodoLast = TodoM Last

type TodoMaybe = Entity TodoId (TodoM Maybe)

deriving instance Eq (TodoM Identity)

deriving instance Show (TodoM Identity)

instance Arbitrary (TodoM Identity) where
  arbitrary = TodoM <$> arbitrary <*> arbitrary

deriving instance Eq (TodoM Maybe)

deriving instance Show (TodoM Maybe)

instance Arbitrary (TodoM Maybe) where
  arbitrary = TodoM <$> arbitrary <*> arbitrary

instance Semigroup TodoLast where
  TodoM d1 c1 <> TodoM d2 c2 = TodoM (d1 <> d2) (c1 <> c2)

newtype CreateTodoRequest = CreateTodoRequest
  { ctrDescription :: Text
  }
  deriving (Eq, Show, Generic)

instance Arbitrary CreateTodoRequest where
  arbitrary = CreateTodoRequest <$> arbitrary

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
              TodoM
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
  let existingLast = convertTodoM @Identity @Last (Last . Just) $ record existing
      reqLast = convertTodoM @Maybe @Last Last $ record req
  todoRecord <-
    existingLast <> reqLast
      & toTodo getLast
      & throwIfNothing MissingFields
  repoUpdate $ Entity existingId todoRecord

logicDelete :: (Repo m, MonadError DeleteError m) => TodoId -> m ()
logicDelete i = do
  _existing <- repoGetById i >>= throwIfNothing NotExists
  repoDelete i

toTodo :: (Applicative g) => (forall a. Field f a -> g a) -> TodoM f -> g (TodoM Identity)
toTodo f TodoM {..} =
  TodoM
    <$> f description
    <*> f completed

convertTodoM :: forall f g. (forall a. Field f a -> Field g a) -> TodoM f -> TodoM g
convertTodoM f TodoM {..} =
  TodoM
    { description = f @Text description,
      completed = f @Bool completed
    }
