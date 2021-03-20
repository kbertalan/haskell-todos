module Main where

import App
import CLI
import Options
import qualified Options.Applicative as O

main :: IO ()
main =
  O.execParser (commandInfo defaultAppOptions) >>= \case
    Server options -> App.run options
