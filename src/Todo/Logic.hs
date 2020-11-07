module Todo.Logic
  ( Repo
  , selectAll
  , insert
  , update
  , getById
  , logicCreate
  , logicUpdate
  , logicPatch
  ) where

import Control.Monad.Except
import Control.Monad.Random.Class
import Data.Coerce                (coerce)
import Data.Monoid                (Last (Last), getLast)
import Data.UUID                  (UUID)
import Todo.Domain

class Repo m where
  selectAll :: m [Todo]
  insert :: Todo -> m Todo
  update :: Todo -> m Todo
  getById :: UUID -> m (Maybe Todo)

logicCreate :: (Repo m, MonadRandom m) => CreateTodoRequest -> m Todo
logicCreate req = do
  newId <- getRandom
  let todo = Todo {
      Todo.Domain.id = newId
    , description = ctrDescription req
    , completed = False
    }
  insert todo

logicUpdate :: (Repo m, MonadError ModifyError m) => Todo -> m Todo
logicUpdate todo = do
  getById (Todo.Domain.id todo) >>= \case
    Nothing -> throwError ModifyNotExists
    Just  _ -> update todo


logicPatch :: (Repo m, MonadError PatchError m) => TodoMaybe -> m Todo
logicPatch req = do
  existingId <- maybe (throwError MissingId) return $ mId req
  maybeExisting <- getById existingId
  existing <- maybe (throwError PatchNotExists) return maybeExisting
  let existingLast = fromTodo (Last . Just) existing
  let todoLast = coerce req
  todo <- maybe (throwError MissingFields) return $ toTodo getLast $ existingLast <> todoLast
  update todo

