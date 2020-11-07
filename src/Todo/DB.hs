module Todo.DB
  ( dbGetById
  , dbSelectAll
  , dbInsert
  , dbUpdate
  ) where

import           Control.Monad.IO.Class
import           Data.Functor.Contravariant ((>$<))
import           Data.Text.Lazy             as L
import           Data.UUID                  (UUID)
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import           Hasql.Statement

import           App.DB                     as DB
import           Todo.Domain                as Todo

dbGetById :: (MonadIO m, WithDB m) => UUID -> m (Maybe Todo)
dbGetById identifier = DB.run $ statement identifier $
  Statement
    "select id, description, completed from todo where id = $1"
    encoder
    decoder
    True
  where
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.rowMaybe row

dbSelectAll:: (MonadIO m, WithDB m) => m [Todo]
dbSelectAll = do
  DB.run $ statement () $
    Statement
      "select id, description, completed from todo order by created_at asc"
      E.noParams
      decoder
      True
    where
      decoder = D.rowList row

dbInsert :: (MonadIO m, WithDB m) => Todo -> m Todo
dbInsert todo = do
  DB.run $ statement todo $
    Statement
      "insert into todo (id, description, completed, created_at, last_updated_at)\
      \ values ($1, $2, $3, now(), now())"
      encoder
      D.noResult
      True
  return todo
  where
    encoder =
      (Todo.id >$< E.param (E.nonNullable E.uuid))
      <> (toStrict . description >$< E.param (E.nonNullable E.text))
      <> (completed >$< E.param (E.nonNullable E.bool))

dbUpdate :: (MonadIO m, WithDB m) => Todo -> m Todo
dbUpdate todo = do
  DB.run $ statement todo $
    Statement
      "update todo set\
      \ description = $2,\
      \ completed = $3,\
      \ last_updated_at = now()\
      \ where id = $1"
      encoder
      D.noResult
      True
  return todo
  where
    encoder =
      (Todo.id >$< E.param (E.nonNullable E.uuid))
      <> (toStrict . description >$< E.param (E.nonNullable E.text))
      <> (completed >$< E.param (E.nonNullable E.bool))

row :: D.Row Todo
row = Todo
  <$> D.column (D.nonNullable D.uuid)
  <*> fmap fromStrict (D.column (D.nonNullable D.text))
  <*> D.column (D.nonNullable D.bool)

