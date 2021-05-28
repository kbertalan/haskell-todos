import qualified App
import Env
import Options

main :: IO ()
main = do
  appOptions <- parseEnv $ appEnv defaultAppOptions
  App.lambda appOptions
