{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Domain
  ( TodoM(..)
  , Todo
  , TodoMaybe
  , TodoLast
  , mkTodo
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
import Control.Monad.Identity (Identity)
import Control.Monad.Random   (MonadRandom, getRandom)
import Data.Aeson             (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericToEncoding,
                               parseJSON, toEncoding, withObject, (.:), (.:?))
import Data.Monoid            (Last (..), getLast)
import Data.Text.Lazy         (Text)
import Data.UUID              (UUID)
import GHC.Generics           (Generic)
import Prelude                hiding (id)

type family Field m a where
  Field Identity a = a
  Field m a = m a

data TodoM m = TodoM
  { identifier  :: Field m UUID
  , description :: Field m Text
  , completed   :: Field m Bool
  }
  deriving Generic

type Todo = TodoM Identity
type TodoLast = TodoM Last
type TodoMaybe = TodoM Maybe

deriving instance Eq Todo
deriving instance Show Todo

instance ToJSON Todo where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = \case
      "identifier" -> "id"
      a -> a
    }

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \v -> TodoM
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
mkTodo = TodoM

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
      identifier = newId
    , description = ctrDescription req
    , completed = False
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
  existingId <- throwIfNothing MissingId $ identifier req
  when (i /= existingId) $ throwError PatchIdentifierMismatch
  existing <- repoGetById existingId >>= throwIfNothing PatchNotExists
  let existingLast = fromTodo (Last . Just) existing
  todo <- throwIfNothing MissingFields $ toTodo getLast $ existingLast <> convertTodo req
  repoUpdate todo

logicDelete :: (Repo m, MonadError DeleteError m) => UUID -> m ()
logicDelete i = do
  _existing <- repoGetById i >>= throwIfNothing DeleteNotExists
  repoDelete i

fromTodo :: (forall a. a -> Field g a) -> Todo -> TodoM g
fromTodo f TodoM{..} = TodoM
  { identifier = f identifier
  , description = f description
  , completed = f completed
  }

toTodo :: (forall a. Field f a -> Maybe a) -> TodoM f -> Maybe Todo
toTodo f TodoM{..} = TodoM
  <$> f identifier
  <*> f description
  <*> f completed

convertTodo :: TodoMaybe -> TodoLast
convertTodo TodoM{..} = TodoM
  { identifier = Last identifier
  , description = Last description
  , completed = Last completed
  }

