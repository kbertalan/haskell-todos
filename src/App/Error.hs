{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Error handling inspired by Matt Parsons' blog at <https://www.parsonsmatt.org/2020/10/27/plucking_in_plucking_out.html>
module App.Error
  ( throwIfNothing,
    throw,
    catch,
    catchLast,
    OneOf,
    HandlerOf,
  )
where

import Control.Monad.Except (MonadError, throwError)
import Data.Kind (Constraint)

class lil :< big where
  inject :: lil -> big

instance lil :< lil where
  inject = id

instance {-# OVERLAPPING #-} lil :< Either lil other where
  inject = Left

instance {-# OVERLAPPABLE #-} (lil :< other) => lil :< Either not other where
  inject = Right . inject

throw :: (MonadError big m, lil :< big) => lil -> m a
throw = throwError . inject

throwIfNothing :: (MonadError big m, lil :< big) => lil -> Maybe a -> m a
throwIfNothing lil = maybe (throw lil) return

catch :: Either (Either lil other) a -> (lil -> Either other a) -> Either other a
catch action handler = case action of
  Left e -> case e of
    Left lil -> handler lil
    Right other -> Left other
  Right r -> Right r

catchLast :: Either lil a -> (lil -> a) -> a
catchLast action handler = case action of
  Left lil -> handler lil
  Right a -> a

type family OneOf e xs :: Constraint where
  OneOf e (x ': xs) = (x :< e, OneOf e xs)
  OneOf e '[] = ()

type family HandlerOf xs a where
  HandlerOf '[] a = a
  HandlerOf xs a = Either (HandlerOf' xs) a

type family HandlerOf' xs where
  HandlerOf' '[x] = x
  HandlerOf' (x ': xs) = Either x (HandlerOf' xs)
