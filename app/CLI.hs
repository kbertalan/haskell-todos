{-# LANGUAGE RecordWildCards #-}

module CLI (Command (..), commandInfo) where

import App
import App.DB as DB
import App.Metrics as Metrics
import App.Random as Random
import App.Web as Web
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Options.Applicative

newtype Command
  = Server App.Options

commandInfo :: App.Options -> ParserInfo Command
commandInfo defs =
  info (commandOptions defs <**> helper) $
    fullDesc
      <> progDesc "Simple ToDo web app"

commandOptions :: App.Options -> Parser Command
commandOptions defs =
  hsubparser $ command "server" (info (Server <$> appOptions defs) (progDesc "start server"))

appOptions :: App.Options -> Parser App.Options
appOptions App.Options {..} =
  App.Options
    <$> webOptions web
    <*> dbOptions db
    <*> metricsOptions metrics
    <*> randomOptions random

webOptions :: Web.Options -> Parser Web.Options
webOptions Web.Options {..} =
  Web.Options
    <$> port
  where
    port =
      option auto $
        long "http-port"
          <> help "Web server port"
          <> value webPort
          <> showDefault
          <> metavar "INT"

dbOptions :: DB.Options -> Parser DB.Options
dbOptions DB.Options {..} =
  DB.Options
    <$> size
    <*> timeout
    <*> host
    <*> port
    <*> user
    <*> password
    <*> name
  where
    size =
      option auto $
        long "pool-size"
          <> value poolSize
          <> showDefault
          <> help "Amount of connections in the pool"
    timeout =
      fmap fromIntegral $
        option auto $
          long "pool-timeout"
            <> value poolTimeout
            <> showDefault
            <> help "Amount of seconds for which the unused connections are kept open"
    host =
      fmap fromString $
        strOption $
          long "db-host"
            <> value (toString dbHost)
            <> showDefault
            <> help "Server host"
    port =
      option auto $
        long "db-port"
          <> value dbPort
          <> showDefault
          <> help "Server port"
    user =
      fmap fromString $
        strOption $
          long "db-user"
            <> value (toString dbUser)
            <> showDefault
            <> help "Username"
    password =
      fmap fromString $
        strOption $
          long "db-password"
            <> value (toString dbPassword)
            <> help "Password"
    name =
      fmap fromString $
        strOption $
          long "db-name"
            <> value (toString dbName)
            <> showDefault
            <> help "Database name"

metricsOptions :: Metrics.Options -> Parser Metrics.Options
metricsOptions Metrics.Options {path = metricsPath} =
  Metrics.Options
    <$> path
  where
    path =
      fmap fromString $
        strOption $
          long "metrics-path"
            <> value (T.unpack metricsPath)
            <> showDefault
            <> help "path where metrics are exposed"

randomOptions :: Random.Options -> Parser Random.Options
randomOptions Random.Options {seed = randomSeed} =
  Random.Options
    <$> seed
  where
    seed =
      fmap (maybe randomSeed Fixed) $
        optional $
          option auto $
            long "random-seed"
              <> help "Seed for random number generator"

toString :: ByteString -> String
toString = unpack . decodeUtf8
