{-# LANGUAGE DeriveGeneric #-}

module Data.Identifier (Identifier (..)) where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toEncoding, toJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Instances.UUID ()

newtype Identifier a = Identifier
  { unIdentifier :: UUID
  }
  deriving (Show, Eq, Generic)

instance ToJSON (Identifier a) where
  toJSON (Identifier uuid) = toJSON uuid
  toEncoding (Identifier uuid) = toEncoding uuid

instance FromJSON (Identifier a) where
  parseJSON v = Identifier <$> parseJSON v

instance Arbitrary (Identifier a) where
  arbitrary = Identifier <$> arbitrary
