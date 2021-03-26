{-# OPTIONS_GHC -fno-warn-orphans #-}

module Todo.JSON where

import Control.Monad.Identity
import Data.Aeson
  ( FromJSON,
    ToJSON,
    object,
    pairs,
    parseJSON,
    toEncoding,
    toJSON,
    withObject,
    (.:),
    (.=),
  )
import Todo.Domain

instance FromJSON (TodoF Identity)

instance ToJSON (TodoF Identity)

instance FromJSON (TodoF Maybe)

instance ToJSON (TodoF Maybe)

instance FromJSON CreateTodoRequest where
  parseJSON = withObject "CreateTodoRequest" $ \v ->
    CreateTodoRequest
      <$> v .: "description"

instance ToJSON CreateTodoRequest where
  toJSON (CreateTodoRequest d) = object ["description" .= d]
  toEncoding (CreateTodoRequest d) = pairs ("description" .= d)
