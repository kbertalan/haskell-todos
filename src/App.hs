{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module App where

import App.DB as DB (Options, migrate, runWithPool)
import App.Ekg as Ekg (Options, runWithEkg, serverMetricsMiddleware)
import App.Log as Log (runWithLog)
import App.Monad (Env (Env), runAppWith)
import App.Random as Random (Options, configure)
import App.Web as Web (Options, run)
import Data.Time.Clock as Time
import Health
import Servant ((:<|>) (..))
import Todo
import Prelude hiding (log)

data Options = Options
  { web :: !Web.Options,
    db :: !DB.Options,
    ekg :: !Ekg.Options,
    random :: !Random.Options
  }
  deriving (Show)

type API = HealthApi :<|> TodoApi

run :: App.Options -> IO ()
run opts =
  Time.getCurrentTime >>= \time ->
    runWithLog $ \log ->
      runWithEkg (ekg opts) $ \ekg ->
        runWithPool (db opts) $ \pool -> do
          Random.configure $ random opts
          migrate pool >>= \case
            Right _ -> return ()
            Left e -> error $ show e

          metricsMiddleware <- serverMetricsMiddleware ekg

          let env = Env pool ekg log time
          currentTime <- Time.getCurrentTime
          putStrLn $ "Started up in " <> show (Time.diffUTCTime currentTime time)

          Web.run @API
            (web opts)
            [metricsMiddleware]
            (healthApi :<|> todoApi)
            (runAppWith env)
