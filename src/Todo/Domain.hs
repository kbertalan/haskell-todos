{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Todo.Domain
  ( Identifier (..),
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

type TodoId = Identifier Todo

data TodoM i m = TodoM
  { identifier :: Field i TodoId,
    description :: Field m Text,
    completed :: Field m Bool
  }
  deriving (Generic)

type Todo = TodoM Identity Identity

type TodoLast = TodoM Identity Last

type TodoMaybe = TodoM Identity Maybe

deriving instance Eq Todo

deriving instance Show Todo

instance Arbitrary Todo where
  arbitrary = TodoM <$> arbitrary <*> arbitrary <*> arbitrary

deriving instance Eq TodoMaybe

deriving instance Show TodoMaybe

instance Arbitrary TodoMaybe where
  arbitrary = TodoM <$> arbitrary <*> arbitrary <*> arbitrary

instance Semigroup TodoLast where
  TodoM _i1 d1 c1 <> TodoM i2 d2 c2 = TodoM i2 (d1 <> d2) (c1 <> c2)

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
  delete :: Identifier Todo -> m (Either DeleteError ())

class Repo m where
  repoSelectPage :: Page -> m [Todo]
  repoInsert :: Todo -> m Todo
  repoUpdate :: Todo -> m Todo
  repoGetById :: Identifier Todo -> m (Maybe Todo)
  repoDelete :: Identifier Todo -> m ()

logicCreate :: (Repo m, MonadRandom m) => CreateTodoRequest -> m Todo
logicCreate req = do
  newId <- getRandom
  let todo =
        TodoM
          { identifier = Identifier newId,
            description = ctrDescription req,
            completed = False
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
  let existingLast = convertTodoM @Identity @Last (Last . Just) existing
      reqLast = convertTodoM @Maybe @Last Last req
  todo <-
    existingLast <> reqLast
      & toTodo getLast
      & throwIfNothing MissingFields
  repoUpdate todo

logicDelete :: (Repo m, MonadError DeleteError m) => Identifier Todo -> m ()
logicDelete i = do
  _existing <- repoGetById i >>= throwIfNothing NotExists
  repoDelete i

toTodo :: (Applicative g) => (forall a. Field f a -> g a) -> TodoM Identity f -> g Todo
toTodo f TodoM {..} =
  TodoM identifier
    <$> f description
    <*> f completed

convertTodoM :: forall f g b. (forall a. Field f a -> Field g a) -> TodoM b f -> TodoM b g
convertTodoM f TodoM {..} =
  TodoM
    { identifier = identifier,
      description = f @Text description,
      completed = f @Bool completed
    }
