{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi
  ) where

import Todo.DB
import Todo.Web

import App.Monad
import Todo.Domain
import Todo.Logic

instance TodoLogic AppM where
  showAll = Todo.Logic.all
  createNew = createNewAction

instance TodoRepo AppM where
  all = selectAll
  add = insert

