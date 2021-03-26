{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi,
    TodoApi,
  )
where

import App.DB (DatabaseT (..))
import App.Log (logDebug)
import App.Monad (AppM)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans (lift)
import Data.Text (Text)
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
import Todo.Web (TodoApi, todoApi)

instance Logic AppM where
  showPage = logged "showPage" . unDB . repoSelectPage
  create = logged "create" . unDB . logicCreate
  modify = logged "modify" . unDB . fmap runExceptT logicUpdate
  patch = logged "path" . unDB . fmap runExceptT logicPatch
  delete = logged "delete" . unDB . fmap runExceptT logicDelete

instance Logic (DatabaseT AppM) where
  showPage = lift . showPage
  create = lift . create
  modify = lift . modify
  patch = lift . patch
  delete = lift . delete

instance Repo (DatabaseT AppM) where
  repoSelectPage = dbSelectPage
  repoInsert = dbInsert
  repoUpdate = dbUpdate
  repoGetById = dbGetById
  repoDelete = dbDeleteById

instance (Repo m, Monad m) => Repo (ExceptT e m) where
  repoSelectPage = lift . repoSelectPage
  repoInsert = lift . repoInsert
  repoUpdate = lift . repoUpdate
  repoGetById = lift . repoGetById
  repoDelete = lift . repoDelete

logged :: Text -> AppM a -> AppM a
logged name action = do
  logDebug $ "starting " <> name
  result <- action
  logDebug $ "ended " <> name
  return result
