{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module App.Monad
  ( AppM,
    runAppWith,
    timed,
    logged,
    tracked,
  )
where

import App.Env (Env)
import App.Log (logger)
import Chronos (stopwatch)
import Chronos.Types (getTimespan)
import Control.DeepSeq (NFData, force)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Data.Text (Text)
import System.Metrics.Prometheus.Metric.Histogram (Histogram, observe)
import UnliftIO (MonadUnliftIO, withRunInIO)

newtype AppM f a = AppM
  { runApp :: ReaderT (Env f (AppM f)) IO a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader (Env f (AppM f)), MonadRandom)

instance MonadUnliftIO (AppM f) where
  withRunInIO go = do
    env <- ask
    liftIO $ go $ runAppWith env

runAppWith :: Env f (AppM f) -> AppM f a -> IO a
runAppWith e a = runReaderT (runApp a) e

timed :: NFData a => Histogram -> AppM f a -> AppM f a
timed histogram action = do
  env <- ask
  (duration, result) <-
    liftIO $
      stopwatch $
        force <$> runAppWith env action
  liftIO $ observe (asMillisecond duration) histogram
  return result
  where
    asMillisecond = (/ 1_000_000) . fromIntegral . getTimespan

logged :: NFData a => Text -> AppM f a -> AppM f a
logged _name action = do
  logger @Text "starting"
  result <- force <$> action
  logger @Text "ended"
  return result

tracked :: (NFData a) => Text -> Histogram -> AppM f a -> AppM f a
tracked name histogram = logged name . timed histogram
