module TestTodoApp where

import           Control.Monad.Except        (ExceptT, runExceptT)
import           Control.Monad.Random.Strict (MonadRandom, RandT, StdGen, evalRandT, mkStdGen)
import           Control.Monad.State.Strict  (MonadState, State, runState)
import qualified Control.Monad.State.Strict  as State
import           Control.Monad.Trans         (lift)
import           Data.Foldable               (find)
import           Todo.Domain

newtype TestTodoM a = TestTodoM
  { runApp :: (RandT StdGen) (State [Todo]) a
  } deriving newtype (Functor, Applicative, Monad, MonadRandom, MonadState [Todo])

instance Logic TestTodoM where
  showAll = repoSelectAll
  create = logicCreate
  modify = fmap runExceptT . logicUpdate
  patch = fmap runExceptT . logicPatch
  delete = fmap runExceptT logicDelete

instance Repo TestTodoM where
  repoSelectAll = State.get

  repoInsert todo = do
    State.modify (todo:)
    return todo

  repoUpdate todo = do
    State.modify replace
    return todo
      where replace [] = []
            replace (a:as) | Todo.Domain.id a == Todo.Domain.id todo = todo : as
                           | otherwise                               = replace as

  repoGetById identifier =
    find ((== identifier) . Todo.Domain.id) <$> State.get

  repoDelete identifier = do
    State.modify $ filter ((/=identifier) . Todo.Domain.id)
    return ()

instance Repo (ExceptT e TestTodoM) where
  repoSelectAll = lift repoSelectAll
  repoInsert = lift . repoInsert
  repoUpdate = lift . repoUpdate
  repoGetById = lift . repoGetById
  repoDelete = lift . repoDelete

testTodo :: TestTodoM a -> StdGen -> [Todo] -> (a, [Todo])
testTodo app =
  runState . evalRandT (runApp app)

testTodoWithSeed :: TestTodoM a -> Int -> [Todo] -> (a, [Todo])
testTodoWithSeed app seed =
  testTodo app $ mkStdGen seed

