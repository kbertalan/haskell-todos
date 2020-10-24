{-# LANGUAGE DeriveGeneric, DeriveAnyClass, QuasiQuotes, TemplateHaskell, RecordWildCards #-}

module Todo
  ( Todo
  , todoApi
  ) where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON, FromJSON)
import Data.Profunctor (dimap)
import Data.Text as T
import Data.Text.Lazy as L
import Data.Tuple.Curry (uncurryN)
import Data.UUID
import Data.Vector (toList)
import GHC.Generics (Generic)
import Hasql.Pool (UsageError)
import Hasql.Session (statement)
import Hasql.TH as TH
import Network.HTTP.Types.Status
import Web.Scotty.Trans

import Env

data Todo = Todo
  { uid :: UUID
  , description :: !T.Text
  , completed :: !Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

asTuple :: Todo -> (UUID, T.Text, Bool)
asTuple Todo {..} = (uid, description, completed)

todoApi :: (WithPool m, MonadIO m) => ScottyT L.Text m ()
todoApi = do
  get "/todo" $
    selectAllTodos >>= \case
      Right r -> json r
      Left  e -> raise $ L.pack $ show e
  post "/todo" $ do
    jsonData >>= insertTodo >>= \case
      Right _ -> status status201
      Left  e -> raise $ L.pack $ show e

selectAllTodos :: (WithPool m) => m (Either UsageError [Todo])
selectAllTodos = usePool $ statement () $
  dimap id (toList . fmap (uncurryN Todo)) [TH.vectorStatement|
    select id :: uuid, description :: text, completed :: bool from todo
    |]

insertTodo :: (WithPool m) => Todo -> m (Either UsageError ())
insertTodo todo = usePool $ statement (asTuple todo) [TH.resultlessStatement|
  insert into todo (id, description, completed) values
    ($1 :: uuid, $2 :: text, $3 :: bool)
  |]

