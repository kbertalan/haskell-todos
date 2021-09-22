{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TodoSpec where

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
import Todo.QuickCheck ()
import Todo.Test (testTodoWithSeed)

testUUID :: TodoId
testUUID = Identifier $ fromJust $ fromString "9474f0eb-06d7-4fd8-b89e-0ce996962508"

spec :: Spec
spec = do
  describe "Create" $
    it "should accept simple description" $
      let request = CreateTodoRequest "desc"
          expectedTodo = Entity testUUID $ TodoF "desc" False
       in testTodoWithSeed (create request) 0 [] `shouldBe` (expectedTodo, [expectedTodo])

  describe "Modify" $ do
    let modifiedTodo = Entity testUUID $ TodoF "desc" False

    it "should fail on missing todo" $
      testTodoWithSeed (modify modifiedTodo) 0 [] `shouldBe` (Left NotExists, [])

    it "should update existing todo" $
      let existingTodo = Entity testUUID $ TodoF "other" True
       in testTodoWithSeed (modify modifiedTodo) 0 [existingTodo] `shouldBe` (Right modifiedTodo, [modifiedTodo])

  describe "Delete" $ do
    it "should fail on missing todo" $
      testTodoWithSeed (delete testUUID) 0 [] `shouldBe` (Left NotExists, [])

    it "should delete existing todo" $
      let existingTodo = Entity testUUID $ TodoF "other" True
       in testTodoWithSeed (delete testUUID) 0 [existingTodo] `shouldBe` (Right (), [])

  describe "Patch" $ do
    let runPatch :: TodoMaybe -> [Todo] -> (Either (Either MissingFields NotExists) Todo, [Todo])
        runPatch patchTodo db = testTodoWithSeed (patch patchTodo) 0 db

    it "should patch existing todo" $
      property $ \testId patchRecord existingRecord ->
        let existingTodo = Entity testId existingRecord
            patchTodo = Entity testId patchRecord
            savedTodo =
              Entity testId $
                TodoF
                  (fromJust (description patchRecord <|> Just (description existingRecord)))
                  (fromJust (completed patchRecord <|> Just (completed existingRecord)))
         in runPatch patchTodo [existingTodo] === (Right savedTodo, [savedTodo])

    it "should fail on not existing todo" $
      let patchTodo = Entity testUUID $ TodoF Nothing Nothing
       in runPatch patchTodo [] `shouldBe` (Left $ Right NotExists, [])

  describe "JSON" $ do
    it "should parse serialized todo" $
      property $ \(todo :: Todo) ->
        Just todo === A.decode (A.encode todo)
    it "should have compatible Todo toJSON and toEncoding" $
      property $ validateToJSON @Todo
    it "should have compatible TodoMaybe toJSON and toEncoding" $
      property $ validateToJSON @TodoMaybe
    it "should have compatible CreateTodoRequest toJSON and toEncoding" $
      property $ validateToJSON @CreateTodoRequest
