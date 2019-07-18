{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.Month where

import Liquidator.Schema.Balance
import Liquidator.Schema.BankBalance
import Liquidator.Schema.RecurringTransaction
import Liquidator.Schema.Transaction
import Liquidator.Schema.Types
import Liquidator.SchemaTH

------------------------------------------------------------------------

data MonthRecurring = MonthRecurring
  { monthRecurring_recurring :: RecurringTransaction
  , monthRecurring_dates :: [Text]
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary MonthRecurring where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''MonthRecurring)

instance ToSchema MonthRecurring where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data Month = Month
  { month_year :: Int32
  , month_month :: Int32
  , month_transactions :: [Transaction]
  , month_recurring :: MonthRecurring
  , month_balance :: [Balance]
  , month_bank_balances :: [BankBalance]
  , month_start_balance :: Int64
  , month_end_balance :: Int64
  , month_lowest_balance :: Int64
  , month_next :: Text
  , month_previous :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary Month where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''Month)

instance ToSchema Month where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
