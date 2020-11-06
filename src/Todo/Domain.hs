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
  , Result
  , Error(..)
  , Logic
  , showAll
  , create
  , modify
  , patch
  ) where

import Data.Aeson     (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Monoid    (Last)
import Data.Text.Lazy
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

newtype Error = Error Text
  deriving (Show)

type Result a = Either Error a

class Logic m where
  showAll :: m (Result [Todo])
  create :: CreateTodoRequest -> m (Result Todo)
  modify :: Todo -> m (Result Todo)
  patch :: TodoMaybe -> m (Result Todo)

