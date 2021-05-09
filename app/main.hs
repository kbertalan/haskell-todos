import App
import CLI (Command (..), commandInfo)
import Env (appEnv, parseEnv)
import Options (defaultAppOptions)
import qualified Options.Applicative as O

main :: IO ()
main = do
  appOptions <- parseEnv $ appEnv defaultAppOptions
  O.execParser (commandInfo appOptions) >>= \case
    Server options -> App.run options
