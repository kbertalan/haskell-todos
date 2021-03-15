{-# LANGUAGE ScopedTypeVariables #-}

module Test.JSON
  ( validateToJSON,
  )
where

import Data.Aeson
import Test.QuickCheck

validateToJSON :: forall a. (FromJSON a, ToJSON a, Eq a, Show a) => a -> Property
validateToJSON a =
  let indirectEncoded = encode $ toJSON a
      directEncoded = encode a
   in decode indirectEncoded === (decode directEncoded :: Maybe a)
