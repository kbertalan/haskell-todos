{-# OPTIONS_GHC -fno-warn-orphans #-}

module Todo.JSON where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    defaultOptions,
    fieldLabelModifier,
    genericToEncoding,
    parseJSON,
    toEncoding,
    withObject,
    (.:),
    (.:?),
  )
import Todo.Domain

instance ToJSON Todo where
  toEncoding =
    genericToEncoding
      defaultOptions
        { fieldLabelModifier = \case
            "identifier" -> "id"
            a -> a
        }

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \v ->
    TodoM
      <$> v .: "id"
      <*> v .: "description"
      <*> v .: "completed"

instance FromJSON TodoMaybe where
  parseJSON = withObject "Todo" $ \v ->
    TodoM
      <$> v .: "id"
      <*> v .:? "description"
      <*> v .:? "completed"

instance FromJSON CreateTodoRequest where
  parseJSON = withObject "CreateTodoRequest" $ \v ->
    CreateTodoRequest
      <$> v .: "description"
