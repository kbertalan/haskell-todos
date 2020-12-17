module App where

import Prelude         hiding (log)

import App.DB          as DB (Options, migrate, runWithDB)
import App.Ekg         as Ekg (Options, runWithEkg)
import App.Log         as Log (runWithLog)
import App.Monad       (Env (Env), runAppWith)
import App.Random      as Random (Options, configure)
import App.Web         as Web (Options, run)
import Data.Time.Clock as Time

import Health          (healthApi)
import Todo            (todoApi)

data Options = Options
  { web    :: !Web.Options
  , db     :: !DB.Options
  , ekg    :: !Ekg.Options
  , random :: !Random.Options
  } deriving (Show)

run :: App.Options -> IO ()
run opts =
  Time.getCurrentTime >>= \time ->
  runWithLog $ \log ->
  runWithEkg (ekg opts) $ \ekg ->
  runWithDB (db opts) $ \db -> do
    Random.configure $ random opts
    migrate db >>= \case
      Right _ -> return ()
      Left e  -> error $ show e

    let env = Env db ekg log time
    currentTime <- Time.getCurrentTime
    putStrLn $ "Started up in " <> show (Time.diffUTCTime currentTime time)

    Web.run (web opts) (runAppWith env) $ do
      healthApi
      todoApi

