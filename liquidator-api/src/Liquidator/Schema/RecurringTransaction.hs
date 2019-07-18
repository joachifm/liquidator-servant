{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.RecurringTransaction where

import Liquidator.SchemaTH
import Liquidator.Schema.TransactionTemplate
import Liquidator.Schema.Types

------------------------------------------------------------------------

data RecurringTransactionBase = RecurringTransactionBase
  { recurringTransactionBase_company_id :: Int64
  , recurringTransactionBase_day_delta :: Int64
  , recurringTransactionBase_month_delta :: Int64
  , recurringTransactionBase_start_date :: Text
  , recurringTransactionBase_end_date :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary RecurringTransactionBase where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''RecurringTransactionBase)

instance ToSchema RecurringTransactionBase where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data RecurringTransaction = RecurringTransaction
  { recurringTransaction_id :: Int64
  , recurringTransaction_template :: TransactionTemplate
  , recurringTransaction_company_id :: Int64
  , recurringTransaction_day_delta :: Int64
  , recurringTransaction_month_delta :: Int64
  , recurringTransaction_start_date :: Text
  , recurringTransaction_end_date :: Text
  , recurringTransaction_transactions :: [Int64]
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary RecurringTransaction where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''RecurringTransaction)

instance ToSchema RecurringTransaction where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data RecurringTransactionCreate = RecurringTransactionCreate
  { recurringTransactionCreate_template :: TransactionTemplateCreate
  , recurringTransactionCreate_company_id :: Int64
  , recurringTransactionCreate_day_delta :: Int64
  , recurringTransactionCreate_month_delta :: Int64
  , recurringTransactionCreate_start_date :: Text
  , recurringTransactionCreate_end_date :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary RecurringTransactionCreate where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''RecurringTransactionCreate)

instance ToSchema RecurringTransactionCreate where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data RecurringTransactionUpdate = RecurringTransactionUpdate
  { recurringTransactionUpdate_id :: Int64
  , recurringTransactionUpdate_template :: TransactionTemplateCreate
  , recurringTransactionUpdate_company_id :: Int64
  , recurringTransactionUpdate_day_delta :: Int64
  , recurringTransactionUpdate_month_delta :: Int64
  , recurringTransactionUpdate_start_date :: Text
  , recurringTransactionUpdate_end_date :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary RecurringTransactionUpdate where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''RecurringTransactionUpdate)

instance ToSchema RecurringTransactionUpdate where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
