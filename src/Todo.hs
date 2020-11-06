{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi
  ) where

import App.Monad
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans  (lift)
import Todo.DB
import Todo.Domain
import Todo.Logic
import Todo.Web

instance Logic AppM where
  showAll = selectAll
  create = logicCreate
  modify = update
  patch  = fmap runExceptT logicPatch

instance Repo AppM where
  selectAll = dbSelectAll
  insert = dbInsert
  update = dbUpdate
  getById = dbGetById

instance Repo (ExceptT Error AppM) where
  selectAll = lift selectAll
  insert = lift . insert
  update = lift . update
  getById = lift . getById

