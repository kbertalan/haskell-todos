{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Todo.TodoSpec where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.Identifier ()
import Data.Maybe (fromJust)
import Data.UUID (fromString)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.JSON (validateToJSON)
import Test.QuickCheck
import Todo.Domain
import Todo.JSON ()
import Todo.Test (testTodoWithSeed)

testUUID :: Identifier Todo
testUUID = Identifier $ fromJust $ fromString "fffd04bd-0ede-42e0-8088-a28c5fba9949"

spec :: Spec
spec = do
  describe "Create" $
    it "should accept simple description" $
      let request = CreateTodoRequest "desc"
          expectedTodo = TodoM testUUID "desc" False
       in testTodoWithSeed (create request) 0 [] `shouldBe` (expectedTodo, [expectedTodo])

  describe "Modify" $ do
    let modifiedTodo = TodoM testUUID "desc" False

    it "should fail on missing todo" $
      testTodoWithSeed (modify modifiedTodo) 0 [] `shouldBe` (Left NotExists, [])

    it "should update existing todo" $
      let existingTodo = TodoM testUUID "other" True
       in testTodoWithSeed (modify modifiedTodo) 0 [existingTodo] `shouldBe` (Right modifiedTodo, [modifiedTodo])

  describe "Delete" $ do
    it "should fail on missing todo" $
      testTodoWithSeed (delete testUUID) 0 [] `shouldBe` (Left NotExists, [])

    it "should delete existing todo" $
      let existingTodo = TodoM testUUID "other" True
       in testTodoWithSeed (delete testUUID) 0 [existingTodo] `shouldBe` (Right (), [])

  describe "Patch" $ do
    let runPatch :: TodoMaybe -> [Todo] -> (Either (Either MissingFields NotExists) Todo, [Todo])
        runPatch patchTodo db = testTodoWithSeed (patch patchTodo) 0 db

    it "should patch existing todo" $
      property $ \testId ->
        forAll (TodoM testId <$> arbitrary <*> arbitrary) $ \patchTodo ->
          forAll (TodoM testId <$> arbitrary <*> arbitrary) $ \existingTodo ->
            let savedTodo =
                  TodoM
                    testId
                    (fromJust (description patchTodo <|> Just (description existingTodo)))
                    (fromJust (completed patchTodo <|> Just (completed existingTodo)))
             in runPatch patchTodo [existingTodo] === (Right savedTodo, [savedTodo])

    it "should fail on not existing todo" $
      let patchTodo = TodoM testUUID Nothing Nothing
       in runPatch patchTodo [] `shouldBe` (Left $ Right NotExists, [])

  describe "json" $ do
    it "should parse serialized todo" $
      property $ \(todo :: Todo) ->
        Just todo === A.decode (A.encode todo)
    it "Todo toJSON is compatible with toEncoding" $
      property $ validateToJSON @Todo
    it "TodoMaybe toJSON is compatible with toEncoding" $
      property $ validateToJSON @TodoMaybe
    it "CreateTodoRequest toJSON is compatible with toEncoding" $
      property $ validateToJSON @CreateTodoRequest
