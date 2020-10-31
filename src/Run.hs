module Run where

import App
import App.DB as DB
import App.Ekg as Ekg
import App.Web as Web

import Health (healthApi)
import Todo (todoApi)

data Options = Options
  { web :: !Web.Options
  , db :: !DB.Options
  , ekg :: !Ekg.Options
  }
  deriving (Show)

run :: Run.Options -> IO ()
run opts =
  Ekg.runWithEkg (ekg opts) $ \ekg ->
    DB.runWithDB (db opts) $ \db -> do
      DB.migrate db >>= \case
        Right _ -> return ()
        Left e -> error $ show e

      let env = Env db ekg
      Web.run (web opts) (runAppWith env) $ do
        healthApi
        todoApi

