{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.Transaction where

import Liquidator.SchemaTH
import Liquidator.Schema.Types

------------------------------------------------------------------------

data TransactionBase = TransactionBase
  { transactionBase_money :: Int64
  , transactionBase_type :: TransactionType
  , transactionBase_description :: Text
  , transactionBase_note :: Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance Arbitrary TransactionBase where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''TransactionBase)

instance ToSchema TransactionBase where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data Transaction = Transaction
  { transaction_id :: Int64
  , transaction_money :: Int64
  , transaction_type :: TransactionType
  , transaction_description :: Text
  , transaction_notes :: Text
  , transaction_company_id :: Int64
  , transaction_recurring_id :: Maybe Int64
  , transaction_date :: Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance Arbitrary Transaction where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''Transaction)

instance ToSchema Transaction where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

instance Semigroup Transaction where
  l <> _ = l

------------------------------------------------------------------------

data TransactionCreate = TransactionCreate
  { transactionCreate_money :: Int64
  , transactionCreate_type :: TransactionType
  , transactionCreate_description :: Text
  , transactionCreate_notes :: Text
  , transactionCreate_company_id :: Int64
  , transactionCreate_recurring_id :: Maybe Int64
  , transactionCreate_date :: Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance Arbitrary TransactionCreate where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''TransactionCreate)

instance ToSchema TransactionCreate where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
