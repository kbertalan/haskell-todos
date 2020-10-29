module Env
  ( Env (..)
  , WithEnv
  , getEnv
  )
where

import App.DB (DB)
import App.Ekg (Ekg)

data Env = Env
  { envDB :: DB
  , envEkg :: Ekg
  }

class WithEnv m where
  getEnv :: m Env

