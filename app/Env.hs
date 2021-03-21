{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Env where

import App
import qualified App.DB as DB
import qualified App.Ekg as Ekg
import qualified App.Random as Random
import qualified App.Web as Web
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

newtype Env a = Env {parseEnv :: IO a}
  deriving (Functor, Applicative)

(?=) :: (Read a) => String -> a -> Env a
(?=) name defaultValue = Env $ do
  value <- lookupEnv name
  let result = readMaybe =<< value
  return $ fromMaybe defaultValue result

appEnv :: App.Options -> Env App.Options
appEnv App.Options {..} =
  App.Options
    <$> webEnv web
    <*> dbEnv db
    <*> ekgEnv ekg
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

ekgEnv :: Ekg.Options -> Env Ekg.Options
ekgEnv Ekg.Options {..} =
  Ekg.Options
    <$> "EKG_HOST" ?= host
    <*> "EKG_PORT" ?= port

randomEnv :: Random.Options -> Env Random.Options
randomEnv Random.Options {..} =
  Random.Options
    <$> "RANDOM_SEED" ?= seed
