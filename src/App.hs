module App where

import Prelude    hiding (log)

import App.DB     as DB
import App.Ekg    as Ekg
import App.Log    as Log
import App.Monad  (Env (Env), runAppWith)
import App.Random as Random
import App.Web    as Web

import Health     (healthApi)
import Todo       (todoApi)

data Options = Options
  { web    :: !Web.Options
  , db     :: !DB.Options
  , ekg    :: !Ekg.Options
  , random :: !Random.Options
  } deriving (Show)

run :: App.Options -> IO ()
run opts =
  Log.runWithLog $ \log ->
    Ekg.runWithEkg (ekg opts) $ \ekg ->
      DB.runWithDB (db opts) $ \db -> do
        Random.configure $ random opts
        DB.migrate db >>= \case
          Right _ -> return ()
          Left e -> error $ show e

        let env = Env db ekg log
        Web.run (web opts) (runAppWith env) $ do
          healthApi
          todoApi

