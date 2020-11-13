{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi
  ) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans  (lift)

import App.Monad            (AppM)
import Todo.DB              (dbDeleteById, dbGetById, dbInsert, dbSelectAll, dbUpdate)
import Todo.Domain          (Logic, create, delete, modify, patch, showAll)
import Todo.Logic           (Repo, delete, getById, insert, logicCreate, logicDelete, logicPatch, logicUpdate,
                             selectAll, update)
import Todo.Web             (todoApi)

instance Logic AppM where
  showAll = selectAll
  create = logicCreate
  modify = fmap runExceptT logicUpdate
  patch  = fmap runExceptT logicPatch
  delete = fmap runExceptT logicDelete

instance Repo AppM where
  selectAll = dbSelectAll
  insert = dbInsert
  update = dbUpdate
  getById = dbGetById
  delete = dbDeleteById

instance Repo (ExceptT e AppM) where
  selectAll = lift selectAll
  insert = lift . insert
  update = lift . update
  getById = lift . getById
  delete = lift . Todo.Logic.delete

