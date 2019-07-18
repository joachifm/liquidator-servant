{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.BankBalance where

import Liquidator.SchemaTH
import Liquidator.Schema.Types

------------------------------------------------------------------------

data BankBalance = BankBalance
  { bankBalance_company_id :: Int64
  , bankBalance_date :: Text
  , bankBalance_money :: Int64
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary BankBalance where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''BankBalance)

instance ToSchema BankBalance where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data BankBalanceCreate = BankBalanceCreate
  { bankBalanceCreate_company_id :: Int64
  , bankBalanceCreate_date :: Text
  , bankBalanceCreate_money :: Int64
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary BankBalanceCreate where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''BankBalanceCreate)

instance ToSchema BankBalanceCreate where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
