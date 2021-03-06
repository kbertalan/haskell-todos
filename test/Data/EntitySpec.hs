{-# LANGUAGE ScopedTypeVariables #-}

module Data.EntitySpec where

import Data.Aeson as A
import Data.Entity
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property, (===))

spec :: Spec
spec =
  describe "Entity" $ do
    it "should read generated json: Int Int" $
      property $ \(entity :: Entity Int Int) ->
        Just entity === A.decode (A.encode entity)
    it "should read generated json: (Maybe Int) [Int]" $
      property $ \(entity :: Entity (Maybe Int) [Int]) ->
        Just entity === A.decode (A.encode entity)
    it "should read generated json: String Bool" $
      property $ \(entity :: Entity String Bool) ->
        Just entity === A.decode (A.encode entity)
