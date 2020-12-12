{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi
  ) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans  (lift)

import App.Monad            (AppM)
import Todo.DB              (dbDeleteById, dbGetById, dbInsert, dbSelectPage, dbUpdate)
import Todo.Domain          (Logic, Repo, create, delete, logicCreate, logicDelete, logicPatch, logicUpdate, modify,
                             patch, repoDelete, repoGetById, repoInsert, repoSelectPage, repoUpdate, showPage)
import Todo.Web             (todoApi)

instance Logic AppM where
  showPage = repoSelectPage
  create = logicCreate
  modify = fmap runExceptT . logicUpdate
  patch  = fmap runExceptT . logicPatch
  delete = fmap runExceptT logicDelete

instance Repo AppM where
  repoSelectPage = dbSelectPage
  repoInsert = dbInsert
  repoUpdate = dbUpdate
  repoGetById = dbGetById
  repoDelete = dbDeleteById

instance Repo (ExceptT e AppM) where
  repoSelectPage = lift . dbSelectPage
  repoInsert = lift . repoInsert
  repoUpdate = lift . repoUpdate
  repoGetById = lift . repoGetById
  repoDelete = lift . repoDelete

