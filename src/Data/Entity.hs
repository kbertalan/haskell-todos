{-# LANGUAGE DeriveGeneric #-}

module Data.Entity
  ( Entity (..),
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (..),
    object,
    parseJSON,
    toJSON,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.HashMap.Strict (toList)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary, arbitrary)

data Entity i a = Entity
  { identifier :: i,
    record :: a
  }
  deriving (Generic, Show, Eq)

instance (Arbitrary i, Arbitrary a) => Arbitrary (Entity i a) where
  arbitrary = Entity <$> arbitrary <*> arbitrary

instance (FromJSON i, FromJSON a) => FromJSON (Entity i a) where
  parseJSON = withObject "Entity" $ \o -> do
    identifier <- o .: "id"
    record <- o .:? "record"
    case record of
      Just v -> return $ Entity identifier v
      Nothing -> Entity identifier <$> parseJSON (Object o)

instance (ToJSON i, ToJSON a) => ToJSON (Entity i a) where
  toJSON (Entity i a) =
    case toJSON a of
      Object o -> object $ ("id" .= i) : toList o
      other -> object ["id" .= i, "record" .= other]
