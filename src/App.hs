{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module App
  ( app
  ) where

import Data.Int
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Options.Applicative
import Hasql.Session as DB
import Hasql.Pool as Pool
import Hasql.OptparseApplicative as DB
import Hasql.TH as TH
import Web.Scotty as S

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
      get "/hello" $ S.json ["hello"::String]
      get "/bello" $ do
        lit <- liftIO $ use pool selectLiteral
        case lit of
          Left e -> liftIO (print e) >> S.json ("Error"::String)
          Right l -> S.json l

selectLiteral :: DB.Session Int64
selectLiteral = DB.statement () $ [TH.singletonStatement|
  select 1 :: int8
  |]
