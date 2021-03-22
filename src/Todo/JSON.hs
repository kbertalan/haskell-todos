{-# OPTIONS_GHC -fno-warn-orphans #-}

module Todo.JSON where

import Control.Monad.Identity
import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (..),
    object,
    pairs,
    parseJSON,
    toEncoding,
    toJSON,
    withObject,
    (.:),
    (.=),
  )
import Data.HashMap.Strict (toList)
import Data.Vector (fromList)
import Todo.Domain

instance (FromJSON i, FromJSON a) => FromJSON (Entity i a) where
  parseJSON = withObject "Entity" $ \v ->
    Entity
      <$> v .: "id"
      <*> parseJSON (Object v)

instance (ToJSON i, ToJSON a) => ToJSON (Entity i a) where
  toJSON (Entity i a) =
    case toJSON a of
      Object o -> object $ ("id" .= i) : toList o
      other -> Array $ fromList [toJSON i, other]

instance FromJSON (TodoM Identity)

instance ToJSON (TodoM Identity)

instance FromJSON (TodoM Maybe)

instance ToJSON (TodoM Maybe)

instance FromJSON CreateTodoRequest where
  parseJSON = withObject "CreateTodoRequest" $ \v ->
    CreateTodoRequest
      <$> v .: "description"

instance ToJSON CreateTodoRequest where
  toJSON (CreateTodoRequest d) = object ["description" .= d]
  toEncoding (CreateTodoRequest d) = pairs ("description" .= d)
