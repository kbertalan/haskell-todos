{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Log
  ( LogAction,
    runWithLog,
    runWithSyncLog,
    runWithDisabledLog,
    withLog,
    withLogContext,
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
import Control.Monad.Reader (MonadReader (local), asks)
import Data.Has (Has, Over (over), obtain)
import Data.Text (Text)
import Prelude hiding (log)

type Log m = LogAction m Text

runWithLog :: (MonadIO m) => (Log m -> IO a) -> IO a
runWithLog = withBackgroundLogger defCapacity logTextStdout

runWithSyncLog :: (MonadIO m) => (Log m -> IO a) -> IO a
runWithSyncLog action = action logTextStdout

runWithDisabledLog :: (MonadIO m) => (Log m -> IO a) -> IO a
runWithDisabledLog action = action $ LogAction $ \_ -> pure ()

withLog ::
  MonadReader env m =>
  Over (LogAction m msg) (LogAction m msg) env env =>
  (LogAction m msg -> LogAction m msg) ->
  m a ->
  m a
withLog f = local (over f)

withLogContext ::
  MonadReader env m =>
  Over (LogAction m Text) (LogAction m Text) env env =>
  Text ->
  m a ->
  m a
withLogContext ctx = withLog $ cmap (formatCtx <>)
  where
    formatCtx = "[ctx:" <> ctx <> "] "

logger :: forall msg env m. (MonadReader env m, Has (LogAction m msg) env) => msg -> m ()
logger msg = do
  LogAction log <- asks obtain
  log msg

data TraceLogged
  = Started
  | Ended

instance Has (LogAction m Text) env => Has (LogAction m TraceLogged) env where
  obtain = cmap formatTraceLogged . obtain

formatTraceLogged :: TraceLogged -> Text
formatTraceLogged = \case
  Started -> "[trace:started] "
  Ended -> "[trace:ended  ] "

logged ::
  NFData a =>
  MonadReader env m =>
  Has (Log m) env =>
  m a ->
  m a
logged action = do
  logger Started
  result <- force <$> action
  logger Ended
  return result
