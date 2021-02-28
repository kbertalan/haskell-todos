{-# LANGUAGE DeriveGeneric #-}

module Data.Identifier (Identifier (..)) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toEncoding)
import Data.UUID (UUID)
import GHC.Generics (Generic)

newtype Identifier a = Identifier
  { unIdentifier :: UUID
  }
  deriving (Show, Eq, Generic)

instance ToJSON (Identifier a) where
  toEncoding (Identifier uuid) = toEncoding uuid

instance FromJSON (Identifier a) where
  parseJSON v = Identifier <$> parseJSON v
