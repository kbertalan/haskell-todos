module DevelMain
  ( update,
  )
where

import App
import Control.Exception (bracket_)
import Options

update :: IO ()
update =
  bracket_
    (putStrLn "Starting")
    (putStrLn "Stopping")
    (App.run defaultAppOptions)
