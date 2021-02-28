{-# LANGUAGE DeriveGeneric #-}

module Data.Identifier (Identifier (..)) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toEncoding)
import Data.UUID (UUID)
import GHC.Generics (Generic)

newtype Identifier = Identifier
  { unIdentifier :: UUID
  }
  deriving (Show, Eq, Generic)

instance ToJSON Identifier where
  toEncoding (Identifier uuid) = toEncoding uuid

instance FromJSON Identifier where
  parseJSON v = Identifier <$> parseJSON v
