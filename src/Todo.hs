{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi
  ) where

import Todo.DB
import Todo.Web

import App.Monad
import Todo.Domain
import Todo.Logic

instance Logic AppM where
  showAll = selectAll
  create = logicCreate
  modify = update

instance Repo AppM where
  selectAll = dbSelectAll
  insert = dbInsert
  update = dbUpdate

