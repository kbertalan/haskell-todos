module Todo.Logic
  ( Repo
  , selectAll
  , insert
  , update
  , logicCreate
  ) where

import Control.Monad.Random.Class
import Todo.Domain

class Repo m where
  selectAll :: m (Result [Todo])
  insert :: Todo -> m (Result Todo)
  update :: Todo -> m (Result Todo)

logicCreate :: (Repo m, MonadRandom m) => CreateTodoRequest -> m (Result Todo)
logicCreate req = do
  newId <- getRandom
  let todo = Todo {
      Todo.Domain.id = newId
    , description = ctrDescription req
    , completed = False
    }
  insert todo

