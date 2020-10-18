{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module App
  ( app
  , App.Options(..)
  ) where

import Data.Aeson.Types as A
import Data.Text.Lazy as T
import Control.Exception (bracket)
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import Hasql.Pool as Pool
import Network.HTTP.Types.Status
import Web.Scotty as S

import Health (healthApi)
import Todo (todoApi)

data Options = Options
  { port :: !Int
  , db :: Pool.Settings
  }
  deriving (Show)

app :: App.Options -> IO ()
app opts = do
  bracket (Pool.acquire $ db opts) Pool.release $ \pool ->
    scotty (port opts) $ do
      defaultHandler $ \msg -> do
        liftIO $ print msg
        status status500
        S.json $ App.Error msg

      healthApi pool
      todoApi pool

newtype Error = Error
  { message :: Text
  } deriving stock (Generic)
  deriving anyclass (ToJSON)

