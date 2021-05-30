{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi,
    TodoApi,
    M.Metrics,
    M.metrics,
  )
where

import App.DB (runWithConnection)
import App.Monad (AppM, tracked)
import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity)
import Control.Monad.Random (MonadRandom)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import System.Metrics.Prometheus.Metric.Histogram (Histogram)
import Todo.DB (dbDeleteById, dbGetById, dbInsert, dbSelectPage, dbUpdate)
import Todo.Domain
import qualified Todo.Metrics as M
import Todo.Web (TodoApi, todoApi)

instance M.HasRegisteredMetrics f => Logic (AppM f) where
  showPage page = runAction M.showPage "showPage" $ repoSelectPage page
  create request = runAction M.create "create" $ logicCreate request
  modify todo = runAction M.modify "modify" $ runExceptT $ logicUpdate todo
  patch todo = runAction M.patch "path" $ runExceptT $ logicPatch todo
  delete todoId = runAction M.delete "delete" $ runExceptT $ logicDelete todoId

runAction ::
  M.HasRegisteredMetrics f =>
  NFData a =>
  (M.RegisteredMetrics -> Identity Histogram) ->
  Text ->
  TodoM f a ->
  AppM f a
runAction metric name action =
  M.getMetric metric
    >>= \hist -> tracked name hist $ unTodo action

newtype TodoM f a = TodoM
  { unTodo :: AppM f a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadRandom)

instance Repo (TodoM f) where
  repoSelectPage = TodoM . runWithConnection . runReaderT . dbSelectPage
  repoInsert = TodoM . runWithConnection . runReaderT . dbInsert
  repoUpdate = TodoM . runWithConnection . runReaderT . dbUpdate
  repoGetById = TodoM . runWithConnection . runReaderT . dbGetById
  repoDelete = TodoM . runWithConnection . runReaderT . dbDeleteById

instance Repo (ExceptT e (TodoM f)) where
  repoSelectPage = lift . repoSelectPage
  repoInsert = lift . repoInsert
  repoUpdate = lift . repoUpdate
  repoGetById = lift . repoGetById
  repoDelete = lift . repoDelete
