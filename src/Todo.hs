{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi
  ) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans  (lift)

import App.Monad            (AppM)
import Todo.DB              (dbGetById, dbInsert, dbSelectAll, dbUpdate)
import Todo.Domain          (Logic, create, modify, patch, showAll)
import Todo.Logic           (Repo, getById, insert, logicCreate, logicPatch, logicUpdate, selectAll, update)
import Todo.Web             (todoApi)

instance Logic AppM where
  showAll = selectAll
  create = logicCreate
  modify = fmap runExceptT logicUpdate
  patch  = fmap runExceptT logicPatch

instance Repo AppM where
  selectAll = dbSelectAll
  insert = dbInsert
  update = dbUpdate
  getById = dbGetById

instance Repo (ExceptT e AppM) where
  selectAll = lift selectAll
  insert = lift . insert
  update = lift . update
  getById = lift . getById

