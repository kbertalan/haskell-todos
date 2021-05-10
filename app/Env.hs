{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Env where

import App
import qualified App.DB as DB
import qualified App.Metrics as Metrics
import App.Random (Seed (Fixed, New))
import qualified App.Random as Random
import qualified App.Web as Web
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

newtype Env a = Env {parseEnv :: IO a}
  deriving (Functor, Applicative)

(?=) :: (FromEnv a) => String -> a -> Env a
(?=) name defaultValue = Env $ do
  value <- lookupEnv name
  let result = fromEnv =<< value
  return $ fromMaybe defaultValue result

class FromEnv a where
  fromEnv :: String -> Maybe a

instance FromEnv Int where
  fromEnv = readMaybe

instance FromEnv T.Text where
  fromEnv = Just . T.pack

instance FromEnv BS.ByteString where
  fromEnv = Just . T.encodeUtf8 . T.pack

instance FromEnv Random.Seed where
  fromEnv "New" = Just New
  fromEnv "new" = Just New
  fromEnv a = Fixed <$> readMaybe a

appEnv :: App.Options -> Env App.Options
appEnv App.Options {..} =
  App.Options
    <$> webEnv web
    <*> dbEnv db
    <*> metricsEnv metrics
    <*> randomEnv random

webEnv :: Web.Options -> Env Web.Options
webEnv Web.Options {..} =
  Web.Options
    <$> "WEB_PORT" ?= webPort

dbEnv :: DB.Options -> Env DB.Options
dbEnv DB.Options {..} =
  DB.Options
    <$> "POOL_SIZE" ?= poolSize
    <*> "POOL_TIMEOUT" ?= poolTimeout
    <*> "DB_HOST" ?= dbHost
    <*> "DB_PORT" ?= dbPort
    <*> "DB_USER" ?= dbUser
    <*> "DB_PASSWORD" ?= dbPassword
    <*> "DB_NAME" ?= dbName

metricsEnv :: Metrics.Options -> Env Metrics.Options
metricsEnv Metrics.Options {..} =
  Metrics.Options
    <$> "METRICS_PATH" ?= path

randomEnv :: Random.Options -> Env Random.Options
randomEnv Random.Options {..} =
  Random.Options
    <$> "RANDOM_SEED" ?= seed
