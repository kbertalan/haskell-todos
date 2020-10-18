{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module App
  ( app
  ) where

import Data.Aeson.Types as A
import Data.Int
import Data.Text.Lazy as T
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Options.Applicative as O
import GHC.Generics (Generic)
import Hasql.Session as DB
import Hasql.Pool as Pool
import Hasql.OptparseApplicative as DB
import Hasql.TH as TH
import Network.HTTP.Types.Status
import Web.Scotty as S

import Health (healthApi)
import Todo (todoApi)

data AppOptions = AppOptions
  { port :: !Int
  , db :: Pool.Settings
  }
  deriving (Show)

appOptions :: O.Parser AppOptions
appOptions = AppOptions
  <$> option auto
    ( long "port"
    <> help "Web server port"
    <> value 3000
    <> showDefault
    <> metavar "INT"
    )
  <*> DB.poolSettings ("db-" <>)

appInfo = info (appOptions <**> helper)
  $ fullDesc
  <> progDesc "Simple TODO web app"

app :: IO ()
app = do
  opts <- execParser appInfo
  bracket (Pool.acquire $ db opts) Pool.release $ \pool ->
    scotty (port opts) $ do
      defaultHandler $ \message -> do
        liftIO $ print message
        status status500
        S.json $ App.Error message

      healthApi pool
      todoApi pool

newtype Error = Error
  { message :: Text
  } deriving (Generic, ToJSON)

