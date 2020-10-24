module Main where

import App
import Data.String (fromString)
import Options.Applicative as O
import Hasql.Connection as Connection
import Hasql.Pool as Pool

main :: IO ()
main = execParser appInfo >>= app

options :: O.Parser Options
options = Options
  <$> option auto
    ( long "http-port"
    <> help "Web server port"
    <> value 3000
    <> showDefault
    <> metavar "INT"
    )
  <*> poolSettings

poolSettings :: Parser Pool.Settings
poolSettings =
  (,,) <$> size <*> timeout <*> connectionSettings
  where
    size =
      option auto $
        long ("pool-size") <>
        value 2 <>
        showDefault <>
        help "Amount of connections in the pool"
    timeout =
      fmap fromIntegral $
      option auto $
        long ("pool-timeout") <>
        value (10 :: Integer) <>
        showDefault <>
        help "Amount of seconds for which the unused connections are kept open"

connectionSettings :: Parser Connection.Settings
connectionSettings =
  Connection.settings <$> host <*> port <*> user <*> password <*> name
  where
    host =
      fmap fromString $ strOption $
        long ("db-host") <> 
        value "127.0.0.1" <>
        showDefault <>
        help "Server host"
    port =
      option auto $
        long ("db-port") <>
        value 5432 <>
        showDefault <>
        help "Server port"
    user =
      fmap fromString $ strOption $
        long ("db-user") <>
        value "postgres" <>
        showDefault <>
        help "Username"
    password =
      fmap fromString $ strOption $
        long ("db-password") <>
        value "postgres" <>
        showDefault <>
        help "Password"
    name =
      fmap fromString $ strOption $
        long ("db-name") <>
        value "postgres" <>
        showDefault <>
        help "Database name"

appInfo :: ParserInfo Options
appInfo = info (options <**> helper)
  $ fullDesc
  <> progDesc "Simple ToDo web app"

