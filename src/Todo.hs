{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi
  ) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans  (lift)

import App.Monad            (AppM)
import Todo.DB              (dbDeleteById, dbGetById, dbInsert, dbSelectAll, dbUpdate)
import Todo.Domain          (Logic, Repo, create, delete, logicCreate, logicDelete, logicPatch, logicUpdate, modify,
                             patch, repoDelete, repoGetById, repoInsert, repoSelectAll, repoUpdate, showAll)
import Todo.Web             (todoApi)

instance Logic AppM where
  showAll = repoSelectAll
  create = logicCreate
  modify = fmap runExceptT logicUpdate
  patch  = fmap runExceptT logicPatch
  delete = fmap runExceptT logicDelete

instance Repo AppM where
  repoSelectAll = dbSelectAll
  repoInsert = dbInsert
  repoUpdate = dbUpdate
  repoGetById = dbGetById
  repoDelete = dbDeleteById

instance Repo (ExceptT e AppM) where
  repoSelectAll = lift repoSelectAll
  repoInsert = lift . repoInsert
  repoUpdate = lift . repoUpdate
  repoGetById = lift . repoGetById
  repoDelete = lift . repoDelete

