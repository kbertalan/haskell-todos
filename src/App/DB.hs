module App.DB
  ( WithPool
  , Pool
  , PoolResult
  , getPool
  , UsePool
  , usePool
  , Session
  , UsageError
  , S.statement
  ) where

import qualified Hasql.Pool as P
import qualified Hasql.Session as S

type Pool = P.Pool
type PoolResult a = Either UsageError a
type UsageError = P.UsageError
type Session = S.Session

class WithPool m where
  getPool :: m P.Pool

class UsePool m where
  usePool :: Session a -> m (PoolResult a)

