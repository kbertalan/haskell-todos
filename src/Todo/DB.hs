module Todo.DB
  ( selectAll
  , insert
  ) where

import Control.Monad.IO.Class
import Data.Bifunctor (first, second)
import Data.Functor.Contravariant ((>$<))
import Data.Text.Lazy as L
import Hasql.Statement
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import App.DB as DB
import Todo.Domain as Todo

selectAll:: (MonadIO m, WithDB m) => m (Todo.Result [Todo])
selectAll = convertError $ DB.run $ statement () $
  Statement
    "select id, description, completed from todo order by created_at asc"
    E.noParams
    decoder
    True
  where
    decoder = D.rowList row

insert :: (MonadIO m, WithDB m) => Todo -> m (Todo.Result Todo)
insert todo =
  fmap (second $ const todo) $ convertError $ DB.run $ statement todo $
    Statement
      "insert into todo (id, description, completed, created_at, last_updated_at)\
      \ values ($1, $2, $3, now(), now())"
      encoder
      D.noResult
      True
  where
    encoder =
      (Todo.id >$< E.param (E.nonNullable E.uuid))
      <> (toStrict . description >$< E.param (E.nonNullable E.text))
      <> (completed >$< E.param (E.nonNullable E.bool))

convertError :: (Monad m) => m (DB.Result a) -> m (Todo.Result a)
convertError = fmap (first $ Error . L.pack . show)

row :: D.Row Todo
row = Todo
  <$> D.column (D.nonNullable D.uuid)
  <*> fmap fromStrict (D.column (D.nonNullable D.text))
  <*> D.column (D.nonNullable D.bool)

