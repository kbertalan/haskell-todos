module Todo.Logic
  ( Repo
  , selectAll
  , insert
  , update
  , getById
  , delete
  , logicCreate
  , logicUpdate
  , logicPatch
  , logicDelete
  ) where

import Control.Monad.Except       (MonadError)
import Control.Monad.Random.Class (MonadRandom, getRandom)
import Data.Coerce                (coerce)
import Data.Monoid                (Last (Last), getLast)
import Data.UUID                  (UUID)

import App.Error                  (throwIfNothing)
import Todo.Domain                (CreateTodoRequest (..), DeleteError (..), ModifyError (..), PatchError (..),
                                   Todo (..), TodoM (..), TodoMaybe, fromTodo, toTodo)

class Repo m where
  selectAll :: m [Todo]
  insert :: Todo -> m Todo
  update :: Todo -> m Todo
  getById :: UUID -> m (Maybe Todo)
  delete :: UUID -> m ()

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
logicUpdate todo =
  getById (Todo.Domain.id todo)
    >>= throwIfNothing ModifyNotExists
    >> update todo

logicPatch :: (Repo m, MonadError PatchError m) => TodoMaybe -> m Todo
logicPatch req = do
  existingId <- throwIfNothing MissingId $ mId req
  existing <- getById existingId >>= throwIfNothing PatchNotExists
  let existingLast = fromTodo (Last . Just) existing
  todo <- throwIfNothing MissingFields $ toTodo getLast $ existingLast <> coerce req
  update todo

logicDelete :: (Repo m, MonadError DeleteError m) => UUID -> m ()
logicDelete identifier = do
  _existing <- getById identifier >>= throwIfNothing DeleteNotExists
  delete identifier

