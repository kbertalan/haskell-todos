{-# LANGUAGE TypeOperators #-}

module App where

import App.DB as DB (Options, migrate, runWithDB)
import App.Ekg as Ekg (Options, runWithEkg, serverMetricsMiddleware)
import App.Log as Log (runWithLog)
import App.Monad (Env (Env), runAppWith)
import App.Random as Random (Options, configure)
import App.Web as Web (Options, run)
import Data.Proxy (Proxy (..))
import Data.Time.Clock as Time
import Health
import Servant ((:<|>) (..))
import qualified Servant.Docs as Doc
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
        runWithDB (db opts) $ \db -> do
          Random.configure $ random opts
          migrate db >>= \case
            Right _ -> return ()
            Left e -> error $ show e

          metricsMiddleware <- serverMetricsMiddleware ekg

          let env = Env db ekg log time
          currentTime <- Time.getCurrentTime
          putStrLn $ "Started up in " <> show (Time.diffUTCTime currentTime time)

          Web.run
            (web opts)
            [metricsMiddleware]
            (Proxy :: Proxy API)
            (healthApi :<|> todoApi)
            (runAppWith env)

doc :: IO ()
doc = putStrLn $ Doc.markdown (Doc.docs (Proxy :: Proxy API))
