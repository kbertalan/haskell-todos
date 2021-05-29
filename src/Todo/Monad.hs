module Todo.Monad (TodoM (TodoM), runTodo) where

import App.DB (Connection, WithConnection, runWithConnection)
import App.Monad (AppM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader (ReaderT (ReaderT), runReaderT)

newtype TodoM f a = TodoM
  { unTodo :: ReaderT Connection (AppM f) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadRandom)

deriving instance WithConnection (TodoM f)

runTodo :: TodoM f a -> AppM f a
runTodo = runWithConnection . runReaderT . unTodo
