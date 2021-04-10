{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module App where

import App.DB as DB (Options, migrate, runWithPool)
import App.Ekg as Ekg (Options, runWithEkg, serverMetricsMiddleware)
import App.Log as Log (runWithLog)
import App.Monad (Env (Env), runAppWith)
import App.Random as Random (Options, configure)
import App.Web as Web (Options, run)
import Chronos (Time (getTime), now)
import Health
import Servant ((:<|>) (..))
import Text.Printf (printf)
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
  now >>= \time ->
    runWithLog $ \log ->
      runWithEkg (ekg opts) $ \ekg ->
        runWithPool (db opts) $ \pool -> do
          Random.configure $ random opts
          migrate pool

          metricsMiddleware <- serverMetricsMiddleware ekg

          let env = Env pool ekg log time
          currentTime <- now
          putStrLn $ "Started up in " <> printf "%.4f" (diffTimeInSeconds currentTime time) <> "s"

          Web.run @API
            (web opts)
            [metricsMiddleware]
            (healthApi :<|> todoApi)
            (runAppWith env)

diffTimeInSeconds :: Time -> Time -> Double
diffTimeInSeconds h l = fromIntegral (getTime h - getTime l) / nanoSecondInSecond
  where
    nanoSecondInSecond = 1000000000
