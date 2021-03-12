{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module App.Web
  ( Options (..),
    run,
    WebHandler,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.List (foldl')
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Servant.Errors
import Servant
import Servant.Swagger
import Servant.Swagger.UI

newtype Options = Options
  { webPort :: Int
  }
  deriving (Show)

type WebHandler m = ExceptT ServerError m

run :: forall a m. (HasServer a '[], HasSwagger a, Monad m) => Options -> [Middleware] -> ServerT a (WebHandler m) -> (forall b. m b -> IO b) -> IO ()
run opts middlewares server runner =
  Warp.run (webPort opts) $
    foldl' (.) defaultMiddleWares middlewares $
      serve apiWithSwagger adaptedServer
  where
    defaultMiddleWares = errorMwDefJson
    api :: Proxy a
    api = Proxy
    swaggerDoc = toSwagger api
    apiWithSwagger :: Proxy (SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> a)
    apiWithSwagger = Proxy
    serverWithSwagger = swaggerSchemaUIServerT swaggerDoc :<|> server
    adaptedServer = hoistServer apiWithSwagger adapter serverWithSwagger
    adapter ma = Handler $ ExceptT $ runner $ runExceptT ma
