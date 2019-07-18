{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.TransactionTemplate where

import Liquidator.SchemaTH
import Liquidator.Schema.Types

------------------------------------------------------------------------

data TransactionTemplate = TransactionTemplate
  { transactionTemplate_id :: Int64
  , transactionTemplate_money :: Int64
  , transactionTemplate_type :: TransactionType
  , transactionTemplate_description :: Text
  , transactionTemplate_note :: Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance Arbitrary TransactionTemplate where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''TransactionTemplate)

instance ToSchema TransactionTemplate where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data TransactionTemplateCreate = TransactionTemplateCreate
  { transactionTemplateCreate_money :: Int64
  , transactionTemplateCreate_type :: TransactionType
  , transactionTemplateCreate_description :: Text
  , transactionTemplateCreate_note :: Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance Arbitrary TransactionTemplateCreate where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''TransactionTemplateCreate)

instance ToSchema TransactionTemplateCreate where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
