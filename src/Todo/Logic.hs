module Todo.Logic
  ( TodoRepo
  , Todo.Logic.all
  , add
  , createNewAction
  ) where

import Control.Monad.Random.Class
import Todo.Domain

class TodoRepo m where
  all :: m (Result [Todo])
  add :: Todo -> m (Result Todo)

createNewAction :: (TodoRepo m, MonadRandom m) => CreateTodoRequest -> m (Result Todo)
createNewAction req = do
  newId <- getRandom
  let todo = Todo {
      Todo.Domain.id = newId
    , description = ctrDescription req
    , completed = False
    }
  add todo

