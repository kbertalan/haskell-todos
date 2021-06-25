{-# LANGUAGE ScopedTypeVariables #-}

module App.Log
  ( runWithLog,
    runWithSyncLog,
    runWithDisabledLog,
    Log,
    logger,
  )
where

import Colog
  ( LogAction (LogAction),
    defCapacity,
    logTextStdout,
    withBackgroundLogger,
  )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (MonadReader)
import Data.Has (Has, obtain)
import Data.Text (Text)
import Prelude hiding (log)

type Log m = LogAction m Text

runWithLog :: (MonadIO m) => (Log m -> IO a) -> IO a
runWithLog = withBackgroundLogger defCapacity logTextStdout

runWithSyncLog :: (MonadIO m) => (Log m -> IO a) -> IO a
runWithSyncLog action = action logTextStdout

runWithDisabledLog :: (MonadIO m) => (Log m -> IO a) -> IO a
runWithDisabledLog action = action $ LogAction $ \_ -> pure ()

logger :: forall msg env m. (MonadReader env m, Has (LogAction m msg) env) => msg -> m ()
logger msg = do
  LogAction log <- asks obtain
  log msg
