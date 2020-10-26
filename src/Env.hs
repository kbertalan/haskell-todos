module Env
  ( Env (..)
  , WithEnv
  , getEnv
  )
where

import App.DB (DB)

data Env = Env
  { envDB :: DB
  }

class WithEnv m where
  getEnv :: m Env

