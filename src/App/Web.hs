{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module App.Web
  ( Options (..),
    run,
    WebHandler,
    webApp,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.List (foldl')
import Data.Swagger
import qualified Data.Text as T
import Data.Version
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Servant.Errors
import Paths_haskell_todos as Meta
import Servant
import Servant.Swagger
import Servant.Swagger.UI.ReDoc

newtype Options = Options
  { webPort :: Int
  }
  deriving (Show)

type WebHandler m = ExceptT ServerError m

run :: forall a m. (HasServer a '[], HasSwagger a, Monad m) => Options -> [Middleware] -> ServerT a (WebHandler m) -> (forall b. m b -> IO b) -> IO ()
run opts middlewares server runner =
  Warp.run (webPort opts) $ webApp @a @m middlewares server runner

webApp :: forall a m. (HasServer a '[], HasSwagger a, Monad m) => [Middleware] -> ServerT a (WebHandler m) -> (forall b. m b -> IO b) -> Application
webApp middlewares server runner =
  foldl' (.) defaultMiddleWares middlewares $
    serve apiWithSwagger adaptedServer
  where
    defaultMiddleWares = errorMwDefJson
    api :: Proxy a
    api = Proxy
    swaggerDoc =
      (toSwagger api)
        { _swaggerInfo =
            mempty
              { _infoTitle = "Simple Todo Rest App",
                _infoDescription = Just "PET project for experimenting with Haskell",
                _infoVersion = T.pack $ showVersion Meta.version
              }
        }
    apiWithSwagger :: Proxy (SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> a)
    apiWithSwagger = Proxy
    serverWithSwagger = redocSchemaUIServerT swaggerDoc :<|> server
    adaptedServer = hoistServer apiWithSwagger adapter serverWithSwagger
    adapter ma = Handler $ ExceptT $ runner $ runExceptT ma
