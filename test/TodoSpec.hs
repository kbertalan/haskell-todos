module TodoSpec where

import           Control.Applicative ((<|>))
import           Data.Maybe          (fromJust)
import           Data.Text.Lazy      (fromStrict)
import           Data.UUID           (UUID, fromString, nil)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Hspec          (Spec, describe, it, shouldBe)
import           Test.Hspec.Hedgehog
import           TestTodoApp
import           Todo.Domain

testUUID :: UUID
testUUID = fromJust $ fromString "fffd04bd-0ede-42e0-8088-a28c5fba9949"


spec :: Spec
spec = do
  describe "Create" $
    it "should accept simple description" $
      let
        request = CreateTodoRequest "desc"
        expectedTodo = Todo testUUID "desc" False
      in runTodoAppWithSeed (create request) 0 [] `shouldBe` (expectedTodo, [expectedTodo])

  describe "Modify" $ do
    let
      modifiedTodo = Todo testUUID "desc" False

    it "should fail on missing todo" $
      runTodoAppWithSeed (modify testUUID modifiedTodo) 0 [] `shouldBe` (Left ModifyNotExists, [])

    it "should fail on not matching ids" $
      runTodoAppWithSeed (modify nil modifiedTodo) 0 [] `shouldBe` (Left ModifyIdentifierMismatch, [])

    it "should update existing todo" $
      let
        existingTodo = Todo testUUID "other" True
      in
        runTodoAppWithSeed (modify testUUID modifiedTodo) 0 [existingTodo] `shouldBe` (Right modifiedTodo, [modifiedTodo])

  describe "Delete" $ do
    it "should fail on missing todo" $
      runTodoAppWithSeed (delete testUUID) 0 [] `shouldBe` (Left DeleteNotExists, [])

    it "should delete existing todo" $
      let
        existingTodo = Todo testUUID "other" True
      in
        runTodoAppWithSeed (delete testUUID) 0 [existingTodo] `shouldBe` (Right (), [])

  describe "Patch" $ do
    let
      existingTodo = Todo testUUID "description" False

    it "should patch existing todo" $ hedgehog $ do
      txt <- forAll $ Gen.maybe $ Gen.text (Range.linear 0 100) Gen.unicode
      done <- forAll $ Gen.maybe Gen.bool
      let patchTodo = TodoM (Just testUUID) (fromStrict <$> txt) done
          savedTodo = Todo
            testUUID
            (fromJust (fmap fromStrict txt <|> Just (description existingTodo)))
            (fromJust (done <|> Just (completed existingTodo)))
      runTodoAppWithSeed (patch testUUID patchTodo) 0 [existingTodo] === (Right savedTodo, [savedTodo])
    it "should fail on missing id" $
      let
        patchTodo = TodoM Nothing Nothing Nothing
      in
        runTodoAppWithSeed (patch testUUID patchTodo) 0 [] `shouldBe` (Left MissingId, [])
    it "should fail on not matching ids" $
      let
        patchTodo = TodoM (Just nil) Nothing Nothing
      in
        runTodoAppWithSeed (patch testUUID patchTodo) 0 [] `shouldBe` (Left PatchIdentifierMismatch, [])

    it "should fail on not existing todo" $
      let
        patchTodo = TodoM (Just testUUID) Nothing Nothing
      in
        runTodoAppWithSeed (patch testUUID patchTodo) 0 [] `shouldBe` (Left PatchNotExists, [])


