{-# LANGUAGE TypeApplications #-}

module Todo.SwaggerSpec where

import Data.Proxy (Proxy (..))
import Servant.Swagger.Test
import Test.Hspec (Spec)
import Todo.Web

spec :: Spec
spec = validateEveryToJSON $ Proxy @TodoApi
