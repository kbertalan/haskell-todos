{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Todo.Domain
  ( TodoM(..)
  , Todo
  , TodoMaybe
  , TodoLast
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
import Control.Monad.Except   (MonadError)
import Control.Monad.Identity (Identity)
import Control.Monad.Random   (MonadRandom, getRandom)
import Data.Aeson             (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericToEncoding, parseJSON,
                               toEncoding, withObject, (.:), (.:?))
import Data.Function          ((&))
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
      a            -> a
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

newtype CreateTodoRequest = CreateTodoRequest
  { ctrDescription :: Text
  } deriving (Show, Generic)

instance FromJSON CreateTodoRequest where
  parseJSON = withObject "CreateTodoRequest" $ \v -> CreateTodoRequest
    <$> v .: "description"

data ModifyError
  = ModifyNotExists
  deriving (Show, Eq)

data PatchError
  = MissingId
  | MissingFields
  | PatchNotExists
  deriving (Show, Eq)

data DeleteError
  = DeleteNotExists
  deriving (Show, Eq)

class Logic m where
  showPage :: Page -> m [Todo]
  create :: CreateTodoRequest -> m Todo
  modify :: Todo -> m (Either ModifyError Todo)
  patch :: TodoMaybe -> m (Either PatchError Todo)
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

logicUpdate :: (Repo m, MonadError ModifyError m) => Todo -> m Todo
logicUpdate todo = do
  repoGetById (identifier todo)
    >>= throwIfNothing ModifyNotExists
    >> repoUpdate todo

logicPatch :: (Repo m, MonadError PatchError m) => TodoMaybe -> m Todo
logicPatch req = do
  existingId <- identifier req & throwIfNothing MissingId
  existing <- repoGetById existingId >>= throwIfNothing PatchNotExists
  let existingLast = convertTodoM @Identity @Last (Last . Just) existing
      reqLast = convertTodoM @Maybe @Last Last req
  todo <- existingLast <> reqLast
    & toTodo getLast
    & throwIfNothing MissingFields
  repoUpdate todo

logicDelete :: (Repo m, MonadError DeleteError m) => UUID -> m ()
logicDelete i = do
  _existing <- repoGetById i >>= throwIfNothing DeleteNotExists
  repoDelete i

toTodo :: (Applicative g) => (forall a. Field f a -> g a) -> TodoM f -> g Todo
toTodo f TodoM{..} = TodoM
  <$> f identifier
  <*> f description
  <*> f completed

convertTodoM :: forall f g. (forall a. Field f a -> Field g a) -> TodoM f -> TodoM g
convertTodoM f TodoM{..} = TodoM
  { identifier = f @UUID identifier
  , description = f @Text description
  , completed = f @Bool completed
  }

