module Env
  ( Env (..)
  , WithEnv
  , getEnv
  )
where

import App.DB (Pool)

data Env = Env
  { envPool :: Pool
  }

class WithEnv m where
  getEnv :: m Env

