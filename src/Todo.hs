{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi,
    TodoApi,
  )
where

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
  showPage = logged "showPage" . repoSelectPage
  create = logged "create" . logicCreate
  modify = logged "modify" . fmap runExceptT logicUpdate
  patch = logged "path" . fmap runExceptT logicPatch
  delete = logged "delete" . fmap runExceptT logicDelete

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

logged :: Text -> AppM a -> AppM a
logged name action = do
  logDebug $ "starting " <> name
  result <- action
  logDebug $ "ended " <> name
  return result
