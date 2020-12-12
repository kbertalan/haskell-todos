module Todo.DB
  ( dbGetById
  , dbSelectAll
  , dbInsert
  , dbUpdate
  , dbDeleteById
  ) where

import           Control.Monad.IO.Class     (MonadIO)
import           Data.Functor.Contravariant ((>$<))
import           Data.Text.Lazy             as L (fromStrict, toStrict)
import           Data.UUID                  (UUID)
import qualified Hasql.Decoders             as D (Row, bool, column, noResult, nonNullable, rowList, rowMaybe, text,
                                                  uuid)
import qualified Hasql.Encoders             as E (Params, bool, noParams, nonNullable, param, text, uuid)

import           App.DB                     as DB (WithDB, execute, statement)
import           Todo.Domain                as Todo (Todo (..))

dbGetById :: (MonadIO m, WithDB m) => UUID -> m (Maybe Todo)
dbGetById identifier = DB.execute $ statement
    "select id, description, completed from todo where id = $1"
    encoder
    decoder
    identifier
  where
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.rowMaybe row

dbSelectAll:: (MonadIO m, WithDB m) => m [Todo]
dbSelectAll = do
  DB.execute $ statement
      "select id, description, completed from todo order by last_updated_at desc limit 20"
      E.noParams
      decoder
      ()
    where
      decoder = D.rowList row

dbInsert :: (MonadIO m, WithDB m) => Todo -> m Todo
dbInsert todo = do
  DB.execute $ statement
      "insert into todo (id, description, completed, created_at, last_updated_at)\
      \ values ($1, $2, $3, now(), now())"
      todoEncoder
      D.noResult
      todo
  return todo

dbUpdate :: (MonadIO m, WithDB m) => Todo -> m Todo
dbUpdate todo = do
  DB.execute $ statement
      "update todo set\
      \ description = $2,\
      \ completed = $3,\
      \ last_updated_at = now()\
      \ where id = $1"
      todoEncoder
      D.noResult
      todo
  return todo

dbDeleteById :: (MonadIO m, WithDB m) => UUID -> m ()
dbDeleteById identifier = do
  DB.execute $ statement
      "delete from todo where id = $1"
      encoder
      D.noResult
      identifier
  return ()
  where
    encoder = E.param (E.nonNullable E.uuid)

row :: D.Row Todo
row = Todo
  <$> D.column (D.nonNullable D.uuid)
  <*> fmap fromStrict (D.column (D.nonNullable D.text))
  <*> D.column (D.nonNullable D.bool)

todoEncoder :: E.Params Todo
todoEncoder =
  (Todo.id >$< E.param (E.nonNullable E.uuid))
  <> (toStrict . description >$< E.param (E.nonNullable E.text))
  <> (completed >$< E.param (E.nonNullable E.bool))
