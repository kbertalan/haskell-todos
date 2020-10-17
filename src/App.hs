{-# LANGUAGE OverloadedStrings #-}
module App
  ( app
  ) where

import Data.Int
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Options.Applicative
import Hasql.Connection as DB
import Hasql.Statement as DB
import Hasql.Session as DB
import Hasql.Encoders as E
import Hasql.Decoders as D
import Hasql.Pool as Pool
import Hasql.OptparseApplicative as DB
import Web.Scotty

data AppOptions = AppOptions
  { port :: !Int
  , db :: Pool.Settings
  }
  deriving (Show)

appOptions :: Parser AppOptions
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
      get "/hello" $ Web.Scotty.json ["hello"::String]
      get "/bello" $ do
        lit <- liftIO $ use pool selectLiteral
        case lit of
          Left e -> liftIO (print e) >> Web.Scotty.json ("Error"::String)
          Right l -> Web.Scotty.json l

selectLiteral :: DB.Session Int64
selectLiteral = DB.statement () $ Statement "select 1" E.noParams (D.singleRow $ D.column $ D.nonNullable $ D.int8) True
