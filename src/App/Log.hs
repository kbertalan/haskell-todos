{-# LANGUAGE ConstraintKinds #-}

module App.Log
  ( runWithLog
  , Log
  , App.Log.WithLog
  , logDebug
  , logInfo
  , logWarning
  , logError
  , logException
  ) where

import Colog                  (LogAction, Message, WithLog, logDebug, logError, logException, logInfo, logWarning,
                               richMessageAction)
import Control.Monad.IO.Class (MonadIO)

type Log m = LogAction m Message
type WithLog env m = Colog.WithLog env Message m

runWithLog :: (MonadIO m) => (Log m -> IO ()) -> IO ()
runWithLog action = action richMessageAction

