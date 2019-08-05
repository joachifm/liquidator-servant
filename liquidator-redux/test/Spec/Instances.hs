{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Spec.Instances () where

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Arbitrary.Generic

import Liquidator.Types
import Liquidator.Web.Types
import Money (Money, moneyFromAmounts)

------------------------------------------------------------------------
-- TransactionFormData
------------------------------------------------------------------------

instance Arbitrary TransactionFormData where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Show TransactionFormData

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

instance Arbitrary Transaction where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Show Transaction

------------------------------------------------------------------------
-- TransactionTemplate
------------------------------------------------------------------------

instance Arbitrary TransactionTemplate where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Show TransactionTemplate

------------------------------------------------------------------------
-- RecurringTransactionFormData
------------------------------------------------------------------------

instance Arbitrary RecurringTransactionFormData where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Show RecurringTransactionFormData

------------------------------------------------------------------------
-- RecurringTransaction
------------------------------------------------------------------------

instance Arbitrary RecurringTransaction where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Show RecurringTransaction

------------------------------------------------------------------------
-- Money
------------------------------------------------------------------------

instance Arbitrary Money where
  arbitrary = moneyFromAmounts
    <$> elements [ 0 .. 999999 ]
    <*> elements [ 0 .. 99 ]
  shrink = genericShrink
