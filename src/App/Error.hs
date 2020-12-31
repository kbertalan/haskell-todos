{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module App.Error
  ( throwIfNothing
  , throw
  , catch
  , catchLast
  , OneOf
  ) where

import Control.Monad.Except (MonadError, throwError)
import Data.Kind            (Constraint)

type (||) = Either

class lil :< big where
  inject :: lil -> big

instance lil :< lil where
  inject = id

instance {-# overlapping #-} lil :< (lil || other) where
  inject = Left

instance {-# overlappable #-} (lil :< other) => lil :< (not || other) where
  inject = Right . inject

throw :: (MonadError big m, lil :< big) => lil -> m a
throw = throwError . inject

throwIfNothing :: (MonadError big m, lil :< big) => lil -> Maybe a -> m a
throwIfNothing lil = maybe (throw lil) return

catch :: Either (lil || other) a -> (lil -> Either other a) -> Either other a
catch action handler = case action of
  Left e -> case e of
    Left lil    -> handler lil
    Right other -> Left other
  Right r  -> Right r

catchLast :: Either lil a -> (lil -> a) -> a
catchLast action handler = case action of
  Left lil -> handler lil
  Right a  -> a

type family OneOf e xs :: Constraint where
  OneOf e (x ': xs) = (x :< e, OneOf e xs)
  OneOf e '[] = ()
