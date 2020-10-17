{-# LANGUAGE OverloadedStrings #-}
module App
  ( app
  ) where

import Options.Applicative
import Hasql.Connection as DB
import Hasql.OptparseApplicative as DB
import Web.Scotty

data AppOptions = AppOptions
  { port :: !Int
  , db :: DB.Settings
  }
  deriving (Show)

appOptions :: Parser AppOptions
appOptions = AppOptions
  <$> option auto
    ( long "port"
    <> help "Web server port"
    <> value 3000
    <> showDefault
    <> metavar "INT"
    )
  <*> DB.connectionSettings ("db-" <>)

appInfo = info (appOptions <**> helper)
  $ fullDesc
  <> progDesc "Simple TODO web app"

app :: IO ()
app = do
  opts <- execParser appInfo
  scotty (port opts) $
    get "/hello" $ json ["hello"::String]


