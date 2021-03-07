{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

newtype Options = Options
  { webPort :: Int
  }
  deriving (Show)

type WebHandler m = ExceptT ServerError m

run :: (HasServer a '[]) => Options -> [Middleware] -> Proxy a -> ServerT a (WebHandler m) -> (forall b. m b -> IO b) -> IO ()
run opts middlewares api server runner =
  Warp.run (webPort opts) $
    foldl' (.) defaultMiddleWares middlewares $
      serve api adaptedServer
  where
    defaultMiddleWares = errorMwDefJson
    adaptedServer = hoistServer api adapter server
    adapter ma = Handler $ ExceptT $ runner $ runExceptT ma
