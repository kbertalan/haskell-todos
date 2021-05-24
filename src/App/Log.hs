{-# LANGUAGE ConstraintKinds #-}

module App.Log
  ( runWithLog,
    runWithSyncLog,
    runWithDisabledLog,
    Log,
    App.Log.WithLog,
    logDebug,
    logInfo,
    logWarning,
    logError,
    logException,
    withLogContext,
  )
where

import Colog
  ( LogAction (LogAction),
    Message,
    Msg (msgText),
    WithLog,
    cmap,
    defCapacity,
    logDebug,
    logError,
    logException,
    logInfo,
    logWarning,
    richMessageAction,
    withBackgroundLogger,
    withLog,
  )
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

type Log m = LogAction m Message

type WithLog env m = Colog.WithLog env Message m

runWithLog :: (MonadIO m) => (Log m -> IO a) -> IO a
runWithLog = withBackgroundLogger defCapacity richMessageAction

runWithSyncLog :: (MonadIO m) => (Log m -> IO a) -> IO a
runWithSyncLog action = action richMessageAction

runWithDisabledLog :: (MonadIO m) => (Log m -> IO a) -> IO a
runWithDisabledLog action = action $ LogAction $ \_ -> pure ()

withLogContext :: (App.Log.WithLog env m) => Text -> m a -> m a
withLogContext name = withLog $ cmap converter
  where
    converter :: Message -> Message
    converter msg = msg {msgText = name <> ": " <> msgText msg}
