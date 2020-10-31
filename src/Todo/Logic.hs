module Todo.Logic
  ( TodoRepo
  , Todo.Logic.all
  , add
  , createNewAction
  ) where

import Data.UUID (nil)
import Todo.Domain

class TodoRepo m where
  all :: m (Result [Todo])
  add :: Todo -> m (Result Todo)

createNewAction :: (TodoRepo m) => CreateTodoRequest -> m (Result Todo)
createNewAction req =
  let todo = Todo { uid = nil -- TODO generate random id here and remove postgresql random generation
    , description = ctrDescription req
    , completed = False
    }
  in
    add todo

