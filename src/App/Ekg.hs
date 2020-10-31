{-# LANGUAGE RecordWildCards #-}

module App.Ekg
  ( Options(..)
  , Ekg
  , WithEkg
  , getEkg
  , runWithEkg
  ) where

import Control.Concurrent       (killThread)
import Control.Exception        (bracket)
import Data.Text.Encoding       (encodeUtf8)
import Data.Text.Lazy           (Text, toStrict)
import System.Remote.Monitoring as EKG

data Options = Options
  { host :: !Text
  , port :: !Int
  } deriving (Show)

type Ekg = Server

runWithEkg :: Options -> (Ekg -> IO ()) -> IO ()
runWithEkg Options {..} = bracket
  (EKG.forkServer (encodeUtf8 $ toStrict host) port)
  (killThread . serverThreadId)

class WithEkg m where
  getEkg :: m Ekg

