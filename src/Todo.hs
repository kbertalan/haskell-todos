{-# LANGUAGE OverloadedStrings #-}

module Todo
  ( Todo
  , todoApi
  ) where

import Data.Text
import Hasql.Pool as DB
import PostgreSQL.Binary.Data
import Web.Scotty

data Todo = Todo
  { description :: !Text
  , uid :: UUID
  , completed :: !Bool
  } deriving (Show)

todoApi :: DB.Pool -> ScottyM ()
todoApi pool = get "/todo" $ raise "OK"

