{-# OPTIONS_GHC -Wno-orphans #-}

module Todo.QuickCheck where

import Control.Monad.Identity (Identity)
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Instances.Text ()
import Todo.Domain (CreateTodoRequest (..), Entity (..), TodoF (..))

instance (Arbitrary i, Arbitrary a) => Arbitrary (Entity i a) where
  arbitrary = Entity <$> arbitrary <*> arbitrary

instance Arbitrary (TodoF Identity) where
  arbitrary = TodoF <$> arbitrary <*> arbitrary

instance Arbitrary (TodoF Maybe) where
  arbitrary = TodoF <$> arbitrary <*> arbitrary

instance Arbitrary CreateTodoRequest where
  arbitrary = CreateTodoRequest <$> arbitrary
