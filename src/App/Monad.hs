module App.Monad
  ( AppM,
    runAppWith,
  )
where

import App.Env (Env)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
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
