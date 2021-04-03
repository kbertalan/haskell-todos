{-# OPTIONS_GHC -Wno-orphans #-}

module Todo
  ( todoApi,
    TodoApi,
  )
where

import App.DB (Connection, WithConnection, WithPool, getPool, runWithConnection)
import App.Log (logDebug, withLogContext)
import App.Monad (AppM, runAppWith)
import Control.DeepSeq (NFData, force)
import Control.Monad.Except (ExceptT, MonadIO (liftIO), runExceptT)
import Control.Monad.Random (MonadRandom)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
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

newtype TodoM a = TodoM
  { unTodo :: ReaderT Connection AppM a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadRandom)

instance WithPool TodoM where
  getPool = TodoM $ lift getPool

deriving instance WithConnection TodoM

runTodo :: TodoM a -> AppM a
runTodo = runWithConnection . runReaderT . unTodo

instance Logic AppM where
  showPage = tracked "showPage" . runTodo . repoSelectPage
  create = tracked "create" . runTodo . logicCreate
  modify = tracked "modify" . runTodo . runExceptT . logicUpdate
  patch = tracked "path" . runTodo . runExceptT . logicPatch
  delete = tracked "delete" . runTodo . runExceptT . logicDelete

instance Repo TodoM where
  repoSelectPage = dbSelectPage
  repoInsert = dbInsert
  repoUpdate = dbUpdate
  repoGetById = dbGetById
  repoDelete = dbDeleteById

instance Repo (ExceptT e TodoM) where
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
