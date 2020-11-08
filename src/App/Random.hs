module App.Random
  ( Options(..)
  , Seed(..)
  , configure
  ) where

import System.Random (mkStdGen, newStdGen, setStdGen)

data Seed
  = New
  | Fixed Int
  deriving (Show)

newtype Options = Options
  { seed :: Seed
  } deriving (Show)

configure :: Options -> IO ()
configure opts = case seed opts of
  New        -> newStdGen >>= setStdGen
  Fixed seed -> setStdGen $ mkStdGen seed

