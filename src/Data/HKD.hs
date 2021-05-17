{-# LANGUAGE RankNTypes #-}

module Data.HKD
  ( TraversableHKD (..),
  )
where

class TraversableHKD h where
  traverseHKD :: Applicative e => (forall a. f a -> e (g a)) -> h f -> e (h g)
