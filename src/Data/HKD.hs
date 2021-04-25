{-# LANGUAGE RankNTypes #-}

module Data.HKD
  ( FunctorHKD (..),
    TraversableHKD (..),
  )
where

class FunctorHKD h where
  mapHKD :: (forall a. m a -> n a) -> h m -> h n

class FunctorHKD h => TraversableHKD h where
  traverseHKD :: Applicative m => (forall a. m a -> m (n a)) -> h m -> m (h n)
