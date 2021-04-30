{-# LANGUAGE RankNTypes #-}

module Data.HKD
  ( FunctorHKD (..),
    TraversableHKD (..),
  )
where

class FunctorHKD h where
  mapHKD :: (forall a. m a -> n a) -> h m -> h n

class FunctorHKD h => TraversableHKD h where
  traverseHKD :: Applicative e => (forall a. f a -> e (g a)) -> h f -> e (h g)
