{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Todo.Domain
  ( TodoM(..)
  , Todo
  , TodoMaybe
  , TodoLast
  , mkTodo
  , identifier
  , description
  , completed
  , CreateTodoRequest(..)
  , Logic
  , showPage
  , create
  , modify
  , patch
  , delete
  , ModifyError(..)
  , PatchError (..)
  , DeleteError (..)
  , repoGetById
  , repoUpdate
  , repoSelectPage
  , repoInsert
  , repoDelete
  , logicCreate
  , logicUpdate
  , logicPatch
  , logicDelete
  , Repo
  ) where

import App.Error              (throwIfNothing)
import App.Paging             (Page)
import Control.Monad          (when)
import Control.Monad.Except   (MonadError, throwError)
import Control.Monad.Identity (Identity (Identity), runIdentity)
import Control.Monad.Random   (MonadRandom, getRandom)
import Data.Aeson             (FromJSON, Options (fieldLabelModifier), ToJSON, defaultOptions, genericToEncoding,
                               parseJSON, toEncoding, withObject, (.:), (.:?))
import Data.Char              (toLower)
import Data.Coerce            (coerce)
import Data.Monoid            (Last (..), getLast)
import Data.Text.Lazy         (Text)
import Data.UUID              (UUID)
import GHC.Generics           (Generic)
import Prelude                hiding (id)

data TodoM m = TodoM
  { mId          :: m UUID
  , mDescription :: m Text
  , mCompleted   :: m Bool
  }
  deriving Generic

type Todo = TodoM Identity
type TodoLast = TodoM Last
type TodoMaybe = TodoM Maybe

deriving instance Eq Todo
deriving instance Show Todo

instance ToJSON Todo where
  toEncoding = genericToEncoding $ defaultOptions {
    fieldLabelModifier = lowerFirst . drop 1
    }
      where
        lowerFirst []     = []
        lowerFirst (x:xs) = toLower x : xs

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \v -> mkTodo
    <$> v .: "id"
    <*> v .: "description"
    <*> v .: "completed"

instance Semigroup TodoLast where
  TodoM i1 d1 c1 <> TodoM i2 d2 c2 = TodoM (i1 <> i2) (d1 <> d2) (c1 <> c2)

instance FromJSON TodoMaybe where
  parseJSON = withObject "Todo" $ \v -> TodoM
    <$> v .:? "id"
    <*> v .:? "description"
    <*> v .:? "completed"

mkTodo :: UUID -> Text -> Bool -> Todo
mkTodo i d c = TodoM
  { mId = Identity i
  , mDescription = Identity d
  , mCompleted = Identity c
  }

identifier :: Todo -> UUID
identifier = runIdentity . mId

description :: Todo -> Text
description = runIdentity . mDescription

completed :: Todo -> Bool
completed = runIdentity . mCompleted

fromTodo :: (forall a. Identity a -> f a) -> Todo -> TodoM f
fromTodo f TodoM{..} = TodoM
  { mId = f mId
  , mDescription = f mDescription
  , mCompleted = f mCompleted
  }

toTodo :: (forall a. f a -> Maybe a) -> TodoM f -> Maybe Todo
toTodo f TodoM{..} = TodoM
  <$> fmap Identity (f mId)
  <*> fmap Identity (f mDescription)
  <*> fmap Identity (f mCompleted)

newtype CreateTodoRequest = CreateTodoRequest
  { ctrDescription :: Text
  } deriving (Show, Generic)

instance FromJSON CreateTodoRequest where
  parseJSON = withObject "CreateTodoRequest" $ \v -> CreateTodoRequest
    <$> v .: "description"

data ModifyError
  = ModifyNotExists
  | ModifyIdentifierMismatch
  deriving (Show, Eq)

data PatchError
  = MissingId
  | MissingFields
  | PatchNotExists
  | PatchIdentifierMismatch
  deriving (Show, Eq)

data DeleteError
  = DeleteNotExists
  deriving (Show, Eq)

class Logic m where
  showPage :: Page -> m [Todo]
  create :: CreateTodoRequest -> m Todo
  modify :: UUID -> Todo -> m (Either ModifyError Todo)
  patch :: UUID -> TodoMaybe -> m (Either PatchError Todo)
  delete :: UUID -> m (Either DeleteError ())

class Repo m where
  repoSelectPage :: Page -> m [Todo]
  repoInsert :: Todo -> m Todo
  repoUpdate :: Todo -> m Todo
  repoGetById :: UUID -> m (Maybe Todo)
  repoDelete :: UUID -> m ()

logicCreate :: (Repo m, MonadRandom m) => CreateTodoRequest -> m Todo
logicCreate req = do
  newId <- getRandom
  let todo = TodoM {
      mId = pure newId
    , mDescription = pure $ ctrDescription req
    , mCompleted = pure False
    }
  repoInsert todo

logicUpdate :: (Repo m, MonadError ModifyError m) => UUID -> Todo -> m Todo
logicUpdate i todo = do
  when (i /= identifier todo) $ throwError ModifyIdentifierMismatch
  repoGetById (identifier todo)
    >>= throwIfNothing ModifyNotExists
    >> repoUpdate todo

logicPatch :: (Repo m, MonadError PatchError m) => UUID -> TodoMaybe -> m Todo
logicPatch i req = do
  existingId <- throwIfNothing MissingId $ mId req
  when (i /= existingId) $ throwError PatchIdentifierMismatch
  existing <- repoGetById existingId >>= throwIfNothing PatchNotExists
  let existingLast = fromTodo (Last . Just . runIdentity) existing
  todo <- throwIfNothing MissingFields $ toTodo getLast $ existingLast <> coerce req
  repoUpdate todo

logicDelete :: (Repo m, MonadError DeleteError m) => UUID -> m ()
logicDelete i = do
  _existing <- repoGetById i >>= throwIfNothing DeleteNotExists
  repoDelete i

