module Main where

import App
import Options.Applicative as O
import Hasql.OptparseApplicative

main :: IO ()
main = execParser appInfo >>= app

options :: O.Parser Options
options = Options
  <$> option auto
    ( long "port"
    <> help "Web server port"
    <> value 3000
    <> showDefault
    <> metavar "INT"
    )
  <*> poolSettings ("db-" <>)

appInfo = info (options <**> helper)
  $ fullDesc
  <> progDesc "Simple TODO web app"

