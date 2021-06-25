{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Has where

class Has a b where
  obtain :: b -> a

class (Has a c, Has b d) => Over a b c d where
  over :: (a -> b) -> c -> d
