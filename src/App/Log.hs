{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Log
  ( runWithLog,
    runWithSyncLog,
    runWithDisabledLog,
    Log,
    logger,
    logged,
  )
where

import Colog
  ( LogAction (LogAction),
    cmap,
    defCapacity,
    logTextStdout,
    withBackgroundLogger,
  )
import Control.DeepSeq (NFData, force)
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

data TraceLogged
  = Started !Text
  | Ended !Text

instance Has (LogAction m Text) env => Has (LogAction m TraceLogged) env where
  obtain = cmap formatTraceLogged . obtain

formatTraceLogged :: TraceLogged -> Text
formatTraceLogged = \case
  Started name -> "[trace:started] " <> name
  Ended name -> "[trace:ended  ] " <> name

logged ::
  NFData a =>
  MonadReader env m =>
  Has (Log m) env =>
  Text ->
  m a ->
  m a
logged name action = do
  logger $ Started name
  result <- force <$> action
  logger $ Ended name
  return result
