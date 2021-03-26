module Data.Paging (
  Page(..)
  ) where

data Page = Page
  { offset :: !Word
  , limit  :: !Word
  } deriving (Show, Eq)

