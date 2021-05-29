{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi,
    TodoApi,
    M.Metrics,
    M.metrics,
  )
where

import App.Log (logDebug, withLogContext)
import App.Metrics (AppMetrics, metricsRegistered)
import App.Monad (AppM, runAppWith)
import Chronos (Timespan (getTimespan), stopwatch)
import Control.DeepSeq (NFData, force)
import Control.Monad.Except (ExceptT, MonadIO (liftIO), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (lift)
import Data.Has (Has (obtain))
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
import qualified Todo.Metrics as M
import Todo.Monad (TodoM, runTodo)
import Todo.Web (TodoApi, todoApi)

instance (Has (M.Metrics Identity) (f Identity)) => Logic (AppM f) where
  showPage page = metric M.showPage >>= \hist -> tracked "showPage" hist $ runTodo $ repoSelectPage page
  create request = metric M.create >>= \hist -> tracked "create" hist $ runTodo $ logicCreate request
  modify todo = metric M.modify >>= \hist -> tracked "modify" hist $ runTodo $ runExceptT $ logicUpdate todo
  patch todo = metric M.patch >>= \hist -> tracked "path" hist $ runTodo $ runExceptT $ logicPatch todo
  delete todoId = metric M.delete >>= \hist -> tracked "delete" hist $ runTodo $ runExceptT $ logicDelete todoId

metric ::
  forall f b.
  Has M.RegisteredMetrics (f Identity) =>
  (M.RegisteredMetrics -> Identity b) ->
  (AppM f) b
metric g = runIdentity . g . obtain @M.RegisteredMetrics . metricsRegistered <$> asks (obtain @(AppMetrics f))

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
