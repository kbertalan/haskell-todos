import qualified App
import Aws.Lambda
import Aws.Lambda.Wai
import Env
import Options

main :: IO ()
main = do
  appOptions <- parseEnv $ appEnv defaultAppOptions
  runWaiAsProxiedHttpLambda defaultDispatcherOptions Nothing (HandlerName "todos") $ App.lambda appOptions
