module App.Error
  ( throwIfNothing
  ) where

import Control.Monad.Except (MonadError, throwError)

throwIfNothing :: (MonadError e m) => e -> Maybe a -> m a
throwIfNothing err = maybe (throwError err) return

