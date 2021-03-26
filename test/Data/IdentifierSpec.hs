{-# LANGUAGE TypeApplications #-}

module Data.IdentifierSpec where

import Data.Identifier
import Test.Hspec
import Test.JSON (validateToJSON)
import Test.QuickCheck

spec :: Spec
spec =
  describe "Identifier" $
    it "should have consistent ToJSON" $
      property $ validateToJSON @(Identifier ())
