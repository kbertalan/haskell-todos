{-# OPTIONS_GHC -Wno-orphans #-}

module App.Web
  ( Scotty
  , Action
  ) where

import Control.Monad.Trans (lift)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans as S

import App.DB as DB

type Scotty = ScottyT Text
type Action = ActionT Text

instance (Monad m, UsePool m) => UsePool (ActionT Text m) where
  usePool = lift . usePool

