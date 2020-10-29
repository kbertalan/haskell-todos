{-# LANGUAGE RecordWildCards #-}

module App.Ekg
  ( Options(..)
  , Ekg
  , runWithEkg
  ) where

import Control.Concurrent (killThread)
import Control.Exception (bracket)
import Data.Text.Lazy (Text, toStrict)
import Data.Text.Encoding (encodeUtf8)
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
