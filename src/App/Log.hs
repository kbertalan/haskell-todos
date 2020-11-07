{-# LANGUAGE ConstraintKinds #-}

module App.Log
  ( runWithLog
  , Log
  , App.Log.WithLog
  , logInfo
  , logDebug
  , logError
  , logException
  ) where

import Colog
import Control.Monad.IO.Class

type Log m = LogAction m Message
type WithLog env m = Colog.WithLog env Message m

runWithLog :: (MonadIO m) => (Log m -> IO ()) -> IO ()
runWithLog action = action richMessageAction

