{-# LANGUAGE TypeApplications #-}

module TodoSpec where

import           Control.Applicative   ((<|>))
import qualified Data.Aeson            as A
import           Data.Functor.Identity (Identity)
import           Data.Maybe            (fromJust)
import           Data.Text.Lazy        (fromStrict)
import           Data.UUID             (UUID, fromString)
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.Hedgehog
import           TestTodoApp           (testTodoWithSeed)
import           Todo.Domain

testUUID :: UUID
testUUID = fromJust $ fromString "fffd04bd-0ede-42e0-8088-a28c5fba9949"


spec :: Spec
spec = do

  describe "Create" $
    it "should accept simple description" $
      let
        request = CreateTodoRequest "desc"
        expectedTodo = TodoM testUUID "desc" False
      in testTodoWithSeed (create request) 0 [] `shouldBe` (expectedTodo, [expectedTodo])


  describe "Modify" $ do
    let
      modifiedTodo = TodoM testUUID "desc" False

    it "should fail on missing todo" $
      testTodoWithSeed (modify modifiedTodo) 0 [] `shouldBe` (Left NotExists, [])

    it "should update existing todo" $
      let
        existingTodo = TodoM testUUID "other" True
      in
        testTodoWithSeed (modify modifiedTodo) 0 [existingTodo] `shouldBe` (Right modifiedTodo, [modifiedTodo])


  describe "Delete" $ do
    it "should fail on missing todo" $
      testTodoWithSeed (delete testUUID) 0 [] `shouldBe` (Left NotExists, [])

    it "should delete existing todo" $
      let
        existingTodo = TodoM testUUID "other" True
      in
        testTodoWithSeed (delete testUUID) 0 [existingTodo] `shouldBe` (Right (), [])


  describe "Patch" $ do
    let
      existingTodo = TodoM testUUID "description" False

    it "should patch existing todo" $ hedgehog $ do
      txt <- forAll $ Gen.maybe $ Gen.text (Range.linear 0 100) Gen.unicode
      done <- forAll $ Gen.maybe Gen.bool
      let patchTodo = TodoM (Just testUUID) (fromStrict <$> txt) done
          savedTodo = TodoM
            testUUID
            (fromJust (fmap fromStrict txt <|> Just (description existingTodo)))
            (fromJust (done <|> Just (completed existingTodo)))
      testTodoWithSeed (patch patchTodo) 0 [existingTodo] === (Right savedTodo, [savedTodo])

    it "should fail on missing id" $
      let
        patchTodo = TodoM Nothing Nothing Nothing
      in
        testTodoWithSeed (patch patchTodo) 0 [] `shouldBe` (Left MissingId, [])

    it "should fail on not existing todo" $
      let
        patchTodo = TodoM (Just testUUID) Nothing Nothing
      in
        testTodoWithSeed (patch patchTodo) 0 [] `shouldBe` (Left PatchNotExists, [])

  describe "json" $
    it "should parse serialized todo" $ hedgehog $ do
      desc <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
      comp <- forAll Gen.bool
      todo <- forAll $ Gen.constant $ TodoM @Identity testUUID (fromStrict desc) comp
      encoded <- forAll $ Gen.constant $ A.encode todo
      Just todo === A.decode encoded

