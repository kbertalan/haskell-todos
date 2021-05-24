module Debug.Lock where

import Control.Exception (BlockedIndefinitelyOnMVar (BlockedIndefinitelyOnMVar), BlockedIndefinitelyOnSTM (BlockedIndefinitelyOnSTM))
import Control.Monad.IO.Class (liftIO)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (Handler (Handler), catches, throwIO)

hasLocked :: (MonadUnliftIO m) => String -> m a -> m a
hasLocked msg action =
  action
    `catches` [ Handler $ \exc@BlockedIndefinitelyOnMVar -> liftIO (putStrLn ("[MVar]: " ++ msg)) >> throwIO exc,
                Handler $ \exc@BlockedIndefinitelyOnSTM -> liftIO (putStrLn ("[STM]: " ++ msg)) >> throwIO exc
              ]
