module Todo.Logic
  ( Repo
  , selectAll
  , insert
  , update
  , getById
  , logicCreate
  , logicPatch
  ) where

import Control.Monad.Except
import Control.Monad.Random.Class
import Data.Coerce                (coerce)
import Data.Monoid                (Last (Last), getLast)
import Data.UUID                  (UUID)
import Todo.Domain

class Repo m where
  selectAll :: m (Result [Todo])
  insert :: Todo -> m (Result Todo)
  update :: Todo -> m (Result Todo)
  getById :: UUID -> m (Maybe Todo)

logicCreate :: (Repo m, MonadRandom m) => CreateTodoRequest -> m (Result Todo)
logicCreate req = do
  newId <- getRandom
  let todo = Todo {
      Todo.Domain.id = newId
    , description = ctrDescription req
    , completed = False
    }
  insert todo

logicPatch :: (Repo m, MonadError Error m) => TodoMaybe -> m Todo
logicPatch req = do
  existingId <- maybe (throwError $ Error "No id is present in the request") return $ mId req
  maybeExisting <- getById existingId
  existing <- maybe (throwError $ Error "Could not find Todo record with id") return maybeExisting
  let existingLast = fromTodo (Last . Just) existing
  let todoLast = coerce req
  todo <- maybe (throwError $ Error "Could not construct Todo record") return $ toTodo getLast $ existingLast <> todoLast
  updated <- update todo
  case updated of
    Left e  -> throwError e
    Right t -> return t


