module TestTodoApp where

import           App.Paging                  (Page (..))
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
  showPage = repoSelectPage
  create = logicCreate
  modify = fmap runExceptT . logicUpdate
  patch = fmap runExceptT . logicPatch
  delete = fmap runExceptT logicDelete

instance Repo TestTodoM where
  repoSelectPage Page{offset,limit} = take (fromIntegral limit) . drop (fromIntegral offset) <$> State.get

  repoInsert todo = do
    State.modify (todo:)
    return todo

  repoUpdate todo = do
    State.modify replace
    return todo
      where replace [] = []
            replace (a:as) | identifier a == identifier todo = todo : as
                           | otherwise                               = replace as

  repoGetById i =
    find ((== i) . identifier) <$> State.get

  repoDelete i = do
    State.modify $ filter ((/= i) . identifier)
    return ()

instance Repo (ExceptT e TestTodoM) where
  repoSelectPage = lift . repoSelectPage
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

