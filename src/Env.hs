module Env
  ( Env (..)
  , WithPool
  , usePool
  )
where

import Data.Text.Lazy (Text)
import Control.Monad.Trans (lift)
import Hasql.Pool (Pool, UsageError)
import Hasql.Session (Session)
import Web.Scotty.Trans (ActionT)

data Env = Env
  { envPool :: Pool
  }

class WithPool m where
  usePool :: Session a -> m (Either UsageError a)

instance (WithPool m, Monad m) => WithPool (ActionT Text m) where
  usePool = lift . usePool

