{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi,
    TodoApi,
    module M,
  )
where

import App.DB (Connection, WithConnection, WithPool, getPool, runWithConnection)
import App.Log (logDebug, withLogContext)
import App.Metrics (WithMetrics (getMetrics), metricsRegistered)
import App.Monad (AppM, runAppWith)
import Chronos (Timespan (getTimespan), stopwatch)
import Control.DeepSeq (NFData, force)
import Control.Monad.Except (ExceptT, MonadIO (liftIO), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Random (MonadRandom)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import System.Metrics.Prometheus.Metric.Histogram (Histogram, observe)
import Todo.DB (dbDeleteById, dbGetById, dbInsert, dbSelectPage, dbUpdate)
import Todo.Domain
  ( Logic,
    Repo,
    create,
    delete,
    logicCreate,
    logicDelete,
    logicPatch,
    logicUpdate,
    modify,
    patch,
    repoDelete,
    repoGetById,
    repoInsert,
    repoSelectPage,
    repoUpdate,
    showPage,
  )
import Todo.Metrics as M
import Todo.Web (TodoApi, todoApi)

newtype TodoM f a = TodoM
  { unTodo :: ReaderT Connection (AppM f) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadRandom)

instance WithPool (TodoM f) where
  getPool = TodoM $ lift getPool

deriving instance WithConnection (TodoM f)

runTodo :: TodoM f a -> AppM f a
runTodo = runWithConnection . runReaderT . unTodo

instance M.WithTodoMetrics f => Logic (AppM f) where
  showPage page = metric M.showPage >>= \hist -> tracked "showPage" hist $ runTodo $ repoSelectPage page
  create request = metric M.create >>= \hist -> tracked "create" hist $ runTodo $ logicCreate request
  modify todo = metric M.modify >>= \hist -> tracked "modify" hist $ runTodo $ runExceptT $ logicUpdate todo
  patch todo = metric M.patch >>= \hist -> tracked "path" hist $ runTodo $ runExceptT $ logicPatch todo
  delete todoId = metric M.delete >>= \hist -> tracked "delete" hist $ runTodo $ runExceptT $ logicDelete todoId

metric ::
  Functor m =>
  WithTodoMetrics f =>
  WithMetrics m f =>
  (Metrics Identity -> Identity b) ->
  m b
metric f = runIdentity . f . getTodoMetrics . metricsRegistered <$> getMetrics

instance Repo (TodoM f) where
  repoSelectPage = dbSelectPage
  repoInsert = dbInsert
  repoUpdate = dbUpdate
  repoGetById = dbGetById
  repoDelete = dbDeleteById

instance Repo (ExceptT e (TodoM f)) where
  repoSelectPage = lift . repoSelectPage
  repoInsert = lift . repoInsert
  repoUpdate = lift . repoUpdate
  repoGetById = lift . repoGetById
  repoDelete = lift . repoDelete

logged :: (NFData a) => Text -> AppM f a -> AppM f a
logged name action = withLogContext name $ do
  logDebug "starting"
  result <- force <$> action
  logDebug "ended"
  return result

timed :: (NFData a) => Histogram -> AppM f a -> AppM f a
timed histogram action = do
  env <- ask
  (duration, result) <-
    liftIO $
      stopwatch $
        force <$> runAppWith env action
  liftIO $ observe (asMillisecond duration) histogram
  return result
  where
    asMillisecond = (/ 1_000_000) . fromIntegral . getTimespan

tracked :: (NFData a) => Text -> Histogram -> AppM f a -> AppM f a
tracked name histogram = logged name . timed histogram
