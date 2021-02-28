{-# LANGUAGE TypeFamilies #-}

module Data.Field (Field) where

import Control.Monad.Identity (Identity)

type family Field m a where
  Field Identity a = a
  Field m a = m a
