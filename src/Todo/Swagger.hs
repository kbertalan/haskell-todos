{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Todo.Swagger where

import Data.Proxy
import Data.Swagger
import Data.Text.Lazy
import Todo.Domain

instance ToParamSchema TodoId

instance ToSchema TodoId where
  declareNamedSchema p = do
    return $ NamedSchema Nothing $ mempty {_schemaParamSchema = toParamSchema p}

instance ToSchema Todo where
  declareNamedSchema _ = do
    idSchema <- declareSchemaRef $ Proxy @TodoId
    textSchema <- declareSchemaRef $ Proxy @Text
    boolSchema <- declareSchemaRef $ Proxy @Bool
    return $
      NamedSchema
        (Just "Todo")
        mempty
          { _schemaRequired = ["id", "description", "completed"],
            _schemaProperties =
              [ ("id", idSchema),
                ("description", textSchema),
                ("completed", boolSchema)
              ],
            _schemaParamSchema =
              mempty
                { _paramSchemaType = Just SwaggerObject
                }
          }

instance ToSchema TodoMaybe where
  declareNamedSchema _ = do
    NamedSchema _n s <- declareNamedSchema (Proxy @Todo)
    return $ NamedSchema (Just "IncompleteTodo") s {_schemaRequired = ["id"]}

instance ToSchema CreateTodoRequest where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef $ Proxy @Text
    return $
      NamedSchema (Just "CreateTodoRequest") $
        mempty
          { _schemaRequired = ["description"],
            _schemaProperties =
              [ ("description", textSchema)
              ],
            _schemaParamSchema =
              mempty
                { _paramSchemaType = Just SwaggerObject
                }
          }
