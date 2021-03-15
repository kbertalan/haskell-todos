{-# OPTIONS_GHC -fno-warn-orphans #-}

module Todo.JSON where

import Data.Aeson
  ( FromJSON,
    Options,
    ToJSON,
    defaultOptions,
    fieldLabelModifier,
    genericToEncoding,
    genericToJSON,
    object,
    pairs,
    parseJSON,
    toEncoding,
    toJSON,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Todo.Domain

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \v ->
    TodoM
      <$> v .: "id"
      <*> v .: "description"
      <*> v .: "completed"

instance ToJSON Todo where
  toJSON = genericToJSON todoOptions
  toEncoding = genericToEncoding todoOptions

instance FromJSON TodoMaybe where
  parseJSON = withObject "Todo" $ \v ->
    TodoM
      <$> v .: "id"
      <*> v .:? "description"
      <*> v .:? "completed"

instance ToJSON TodoMaybe where
  toJSON = genericToJSON todoOptions
  toEncoding = genericToEncoding todoOptions

todoOptions :: Options
todoOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "identifier" -> "id"
        a -> a
    }

instance FromJSON CreateTodoRequest where
  parseJSON = withObject "CreateTodoRequest" $ \v ->
    CreateTodoRequest
      <$> v .: "description"

instance ToJSON CreateTodoRequest where
  toJSON (CreateTodoRequest d) = object ["description" .= d]
  toEncoding (CreateTodoRequest d) = pairs ("description" .= d)
