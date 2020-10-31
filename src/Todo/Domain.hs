{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards #-}

module Todo.Domain
  ( Todo(..)
  , CreateTodoRequest(..)
  , Result
  , Error(..)
  , TodoLogic
  , showAll
  , createNew
  ) where

import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, object, withObject, (.=), (.:))
import Data.Text.Lazy
import Data.UUID (UUID)
import GHC.Generics (Generic)

data Todo = Todo
  { id :: UUID
  , description :: !Text
  , completed :: !Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

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

class TodoLogic m where
  showAll :: m (Result [Todo])
  createNew :: CreateTodoRequest -> m (Result Todo)

