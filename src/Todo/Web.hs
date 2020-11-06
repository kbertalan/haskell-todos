module Todo.Web
  ( todoApi
  ) where

import           Control.Monad.Trans
import qualified Data.Text.Lazy            as L
import           Network.HTTP.Types.Status
import           Web.Scotty.Trans          as W

import           App.Web
import           Todo.Domain

todoApi :: (MonadIO m, Logic m) => Scotty m ()
todoApi = do
  get "/todo" $
    lift showAll >>= \case
      Right r -> json r
      Left  e -> raise $ L.pack $ show e
  post "/todo" $
    jsonData >>= lift . create >>= \case
      Right r -> do
        status status201
        json r
      Left  e -> raise $ L.pack $ show e
  put "/todo" $
    jsonData >>= lift . modify >>= \case
      Right r -> json r
      Left  e -> raise $ L.pack $ show e
  W.patch "/todo" $
    jsonData >>= lift . Todo.Domain.patch >>= \case
      Right r -> json r
      Left  e -> raise $ L.pack $ show e


