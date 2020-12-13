module App.Time
  ( Time
  , getTime
  , diffTime
  , runWithStartupTime
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Time.Clock        as T (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

type Time = T.UTCTime
type DiffTime = T.NominalDiffTime

getTime :: (MonadIO m) => m Time
getTime = liftIO T.getCurrentTime

diffTime :: Time -> Time -> DiffTime
diffTime = T.diffUTCTime

runWithStartupTime :: (Time -> IO a) -> IO a
runWithStartupTime action =
  getTime >>= action
