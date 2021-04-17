{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module App where

import App.DB      as DB (Options, migrate, runWithPool)
import App.Log     as Log (runWithLog)
import App.Metrics as Metrics (Metrics (metricsEndpoint, metricsMiddleware), Options, runWithMetrics)
import App.Monad   (Env (Env), runAppWith)
import App.Random  as Random (Options, configure)
import App.Web     as Web (Options, run)
import Chronos     (Time (getTime), now)
import Health
import Prelude     hiding (log)
import Servant     ((:<|>) (..))
import Text.Printf (printf)
import Todo

data Options = Options
  { web     :: !Web.Options,
    db      :: !DB.Options,
    metrics :: !Metrics.Options,
    random  :: !Random.Options
  }
  deriving (Show)

type API = HealthApi :<|> TodoApi

run :: App.Options -> IO ()
run opts =
  now >>= \time ->
    runWithLog $ \log ->
      runWithMetrics (metrics opts) [] $ \ms ->
        runWithPool (db opts) $ \pool -> do
          Random.configure $ random opts
          migrate pool

          let env = Env pool ms log time
          currentTime <- now
          putStrLn $ "Started up in " <> printf "%.4f" (diffTimeInSeconds currentTime time) <> "s"

          Web.run @API
            (web opts)
            [metricsEndpoint ms, metricsMiddleware ms]
            (healthApi :<|> todoApi)
            (runAppWith env)

diffTimeInSeconds :: Time -> Time -> Double
diffTimeInSeconds h l = fromIntegral (getTime h - getTime l) / nanoSecondInSecond
  where
    nanoSecondInSecond = 1_000_000_000
