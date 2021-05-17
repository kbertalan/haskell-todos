{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Has where

class Has a b where
  obtain :: b -> a
