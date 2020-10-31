{-# OPTIONS_GHC -Wno-orphans #-}

module Todo 
  ( todoApi
  ) where

import Todo.Web
import Todo.DB

import Todo.Domain
import Todo.Logic
import App

instance TodoLogic AppM where
  showAll = Todo.Logic.all
  createNew = createNewAction

instance TodoRepo AppM where
  all = selectAll
  add = insert

