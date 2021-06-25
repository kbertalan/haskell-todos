module App.Monad
  ( AppM,
    runAppWith,
    withEnv,
    timed,
    tracked,
  )
where

import App.Env (Env)
import App.Log (logged, withLogContext)
import App.Metrics (timed)
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Data.Text (Text)
import System.Metrics.Prometheus.Metric.Histogram (Histogram)
import UnliftIO (MonadUnliftIO, withRunInIO)

type AppEnv f l = Env f l (AppM f l)

newtype AppM f l a = AppM
  { runApp :: ReaderT (AppEnv f l) IO a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader (AppEnv f l), MonadRandom)

instance MonadUnliftIO (AppM f l) where
  withRunInIO go = do
    env <- ask
    liftIO $ go $ runAppWith env

runAppWith :: AppEnv f l -> AppM f l a -> IO a
runAppWith e a = runReaderT (runApp a) e

withEnv :: (AppEnv f l -> AppEnv g m) -> AppM g m a -> AppM f l a
withEnv f action = do
  env <- ask
  liftIO $ runAppWith (f env) action

tracked :: (NFData a) => Text -> Histogram -> AppM f Text a -> AppM f Text a
tracked name histogram = withLogContext name . logged . timed histogram
