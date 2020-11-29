{-# LANGUAGE InstanceSigs #-}

module TestTodoApp where

import           Control.Monad.Except        (ExceptT, runExceptT)
import           Control.Monad.Random.Strict (MonadRandom, RandT, StdGen, evalRandT, mkStdGen)
import           Control.Monad.State.Strict  (MonadState, State, runState)
import qualified Control.Monad.State.Strict  as State
import           Control.Monad.Trans         (lift)
import           Data.Foldable               (find)
import           Data.UUID                   (UUID)
import           Todo.Domain

newtype TestApp a = TestApp
  { runApp :: (RandT StdGen) (State [Todo]) a
  } deriving newtype (Functor, Applicative, Monad, MonadRandom, MonadState [Todo])

instance Logic TestApp where
  showAll :: TestApp [Todo]
  showAll = undefined

  create :: CreateTodoRequest -> TestApp Todo
  create = logicCreate

  modify :: UUID -> Todo -> TestApp (Either ModifyError Todo)
  modify = fmap runExceptT . logicUpdate

  patch :: UUID -> TodoMaybe -> TestApp (Either PatchError Todo)
  patch = fmap runExceptT . logicPatch

  delete :: UUID -> TestApp (Either DeleteError ())
  delete = fmap runExceptT logicDelete

instance Repo TestApp where
  repoSelectAll :: TestApp [Todo]
  repoSelectAll = State.get

  repoInsert :: Todo -> TestApp Todo
  repoInsert todo = do
    State.modify (todo:)
    return todo

  repoUpdate :: Todo -> TestApp Todo
  repoUpdate todo = do
    State.modify replace
    return todo
      where replace [] = []
            replace (a:as) | Todo.Domain.id a == Todo.Domain.id todo = todo : as
                           | otherwise                               = replace as

  repoGetById :: UUID -> TestApp (Maybe Todo)
  repoGetById identifier =
    find ((== identifier) . Todo.Domain.id) <$> State.get

  repoDelete :: UUID -> TestApp ()
  repoDelete identifier = do
    State.modify $ filter ((/=identifier) . Todo.Domain.id)
    return ()

instance Repo (ExceptT e TestApp) where
  repoSelectAll = lift repoSelectAll
  repoInsert = lift . repoInsert
  repoUpdate = lift . repoUpdate
  repoGetById = lift . repoGetById
  repoDelete = lift . repoDelete

runTodoApp :: TestApp a -> StdGen -> [Todo] -> (a, [Todo])
runTodoApp app =
  runState . evalRandT (runApp app)

runTodoAppWithSeed :: TestApp a -> Int -> [Todo] -> (a, [Todo])
runTodoAppWithSeed app seed =
  runTodoApp app $ mkStdGen seed

