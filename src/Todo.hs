{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Todo
  ( Todo
  , todoApi
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Aeson (ToJSON, FromJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Text as T
import Data.Text.Lazy as L
import Data.UUID
import GHC.Generics (Generic)
import Hasql.Statement
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Network.HTTP.Types.Status
import Web.Scotty.Trans

import App.DB as DB
import App.Web as Web

data Todo = Todo
  { uid :: UUID
  , description :: !T.Text
  , completed :: !Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

todoApi :: (WithDB m, MonadIO m) => Scotty m ()
todoApi = do
  get "/todo" $
    selectAllTodos >>= \case
      Right r -> json r
      Left  e -> raise $ L.pack $ show e
  post "/todo" $ do
    jsonData >>= insertTodo >>= \case
      Right _ -> status status201
      Left  e -> raise $ L.pack $ show e

selectAllTodos :: (MonadIO m, WithDB m) => Action m (DB.Result [Todo])
selectAllTodos = lift . DB.run $ statement () $
  Statement "select id, description, completed from todo" E.noParams decoder True
  where
    decoder = D.rowList row
    row = Todo
      <$> D.column (D.nonNullable D.uuid)
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.bool)

insertTodo :: (MonadIO m, WithDB m) => Todo -> Action m (DB.Result ())
insertTodo todo = lift . DB.run $ statement todo $
  Statement "insert into todo (id, description, completed) values ($1, $2, $3)" encoder D.noResult True
  where
    encoder =
      (uid >$< E.param (E.nonNullable E.uuid))
      <> (description >$< E.param (E.nonNullable E.text))
      <> (completed >$< E.param (E.nonNullable E.bool))

