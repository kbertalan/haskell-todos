module Main where

import App
import App.DB              as DB
import App.Ekg             as Ekg
import App.Random          as Random
import App.Web             as Web
import Data.String         (fromString)
import Options.Applicative as O

main :: IO ()
main = execParser appInfo >>= App.run

appInfo :: ParserInfo App.Options
appInfo = info (appOptions <**> helper)
  $ fullDesc
  <> progDesc "Simple ToDo web app"

appOptions :: O.Parser App.Options
appOptions = App.Options
  <$> webOptions
  <*> dbOptions
  <*> ekgOptions
  <*> randomOptions

webOptions :: Parser Web.Options
webOptions = Web.Options
  <$> port
  where
    port = option auto
      $ long "http-port"
      <> help "Web server port"
      <> value 3000
      <> showDefault
      <> metavar "INT"

dbOptions :: Parser DB.Options
dbOptions = DB.Options
  <$> size
  <*> timeout
  <*> host
  <*> port
  <*> user
  <*> password
  <*> name
  where
    size = option auto
      $ long "pool-size"
      <> value 2
      <> showDefault
      <> help "Amount of connections in the pool"
    timeout = fmap fromIntegral
      $ option auto
      $ long "pool-timeout"
      <> value (10 :: Integer)
      <> showDefault
      <> help "Amount of seconds for which the unused connections are kept open"
    host = fmap fromString
      $ strOption
      $ long "db-host"
      <> value "127.0.0.1"
      <> showDefault
      <> help "Server host"
    port = option auto
      $ long "db-port"
      <> value 5432
      <> showDefault
      <> help "Server port"
    user = fmap fromString
      $ strOption
      $ long "db-user"
      <> value "postgres"
      <> showDefault
      <> help "Username"
    password = fmap fromString
      $ strOption
      $ long "db-password"
      <> value "postgres"
      <> showDefault
      <> help "Password"
    name = fmap fromString
      $ strOption
      $ long "db-name"
      <> value "postgres"
      <> showDefault
      <> help "Database name"

ekgOptions :: O.Parser Ekg.Options
ekgOptions = Ekg.Options
  <$> host
  <*> port
  where
    host = fmap fromString
      $ strOption
      $ long "ekg-host"
      <> value "localhost"
      <> showDefault
      <> help "ekg host to bind"
    port = option auto
      $ long "ekg-port"
      <> value 8000
      <> showDefault
      <> help "Ekg port"

randomOptions :: O.Parser Random.Options
randomOptions = Random.Options
  <$> seed
  where
    seed = fmap (maybe New Fixed)
      $ optional
      $ option auto
      $ long "random-seed"
      <> help "Seed for random number generator"

