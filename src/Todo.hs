{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi,
    TodoApi,
  )
where

import App.DB (DatabaseT (..))
import App.Log (logDebug, withLogContext)
import App.Monad (AppM, runAppWith)
import Control.DeepSeq (NFData, force)
import Control.Monad.Except (ExceptT, MonadIO (liftIO), runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Data.Text (Text, pack)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
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
  showPage = tracked "showPage" . unDB . repoSelectPage
  create = tracked "create" . unDB . logicCreate
  modify = tracked "modify" . unDB . fmap runExceptT logicUpdate
  patch = tracked "path" . unDB . fmap runExceptT logicPatch
  delete = tracked "delete" . unDB . fmap runExceptT logicDelete

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

logged :: (NFData a) => Text -> AppM a -> AppM a
logged name action = withLogContext name $ do
  logDebug "starting"
  result <- force <$> action
  logDebug "ended"
  return result

timed :: (NFData a) => AppM a -> AppM a
timed action = do
  env <- ask
  (result, time) <- liftIO $ do
    before <- getCurrentTime
    result <- force <$> runAppWith env action
    after <- getCurrentTime
    return (result, diffUTCTime after before)
  logDebug $ "time: " <> pack (show time)
  return result

tracked :: (NFData a) => Text -> AppM a -> AppM a
tracked name = logged name . timed
