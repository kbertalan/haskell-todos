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
  , ModifyError(..)
  , PatchError (..)
  ) where

import Data.Aeson     (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Monoid    (Last)
import Data.Text.Lazy (Text)
import Data.UUID      (UUID)
import GHC.Generics   (Generic)
import Prelude        hiding (id)

data Todo = Todo
  { id          :: !UUID
  , description :: !Text
  , completed   :: !Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

data TodoM m = TodoM
  { mId          :: m UUID
  , mDescription :: m Text
  , mCompleted   :: m Bool
  } deriving (Generic)

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

instance ToJSON CreateTodoRequest where
  toJSON CreateTodoRequest {..} = object
    [ "description" .= ctrDescription ]

instance FromJSON CreateTodoRequest where
  parseJSON = withObject "CreateTodoRequest" $ \v -> CreateTodoRequest
    <$> v .: "description"

data ModifyError
  = ModifyNotExists
  deriving (Show)

data PatchError
  = MissingId
  | MissingFields
  | PatchNotExists
  deriving Show

class Logic m where
  showAll :: m [Todo]
  create :: CreateTodoRequest -> m Todo
  modify :: Todo -> m (Either ModifyError Todo)
  patch :: TodoMaybe -> m (Either PatchError Todo)

