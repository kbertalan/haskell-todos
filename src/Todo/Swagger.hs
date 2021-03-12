{-# OPTIONS_GHC -fno-warn-orphans #-}

module Todo.Swagger where

import Data.Swagger
import Todo.Domain

instance ToParamSchema TodoId

instance ToParamSchema Todo where
  toParamSchema _ = mempty

instance ToSchema Todo where
  declareNamedSchema _ = return $ NamedSchema (Just "Todo") mempty

instance ToSchema TodoMaybe where
  declareNamedSchema _ = return $ NamedSchema (Just "Todo") mempty

instance ToSchema CreateTodoRequest where
  declareNamedSchema _ = return $ NamedSchema (Just "Create Todo Request") mempty
