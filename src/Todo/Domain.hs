{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Todo.Domain
  ( Todo(..)
  , CreateTodoRequest(..)
  , Result
  , Error(..)
  , Logic
  , showAll
  , create
  , modify
  ) where

import Data.Aeson     (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Text.Lazy
import Data.UUID      (UUID)
import GHC.Generics   (Generic)

data Todo = Todo
  { id          :: !UUID
  , description :: !Text
  , completed   :: !Bool
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

class Logic m where
  showAll :: m (Result [Todo])
  create :: CreateTodoRequest -> m (Result Todo)
  modify :: Todo -> m (Result Todo)


