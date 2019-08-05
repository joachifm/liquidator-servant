{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module SpecInstances () where

import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Liquidator.Types
import Liquidator.Web.Types
import Money (Money, moneyFromAmounts)

------------------------------------------------------------------------
-- TransactionFormData
------------------------------------------------------------------------

instance Arbitrary TransactionFormData where
  arbitrary = TransactionFormData
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

deriving instance Show TransactionFormData

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

instance Arbitrary Transaction where
  arbitrary = Transaction
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

deriving instance Show Transaction

------------------------------------------------------------------------
-- Money
------------------------------------------------------------------------

instance Arbitrary Money where
  arbitrary = moneyFromAmounts
    <$> elements [ 0 .. 999999 ]
    <*> elements [ 0 .. 99 ]
  shrink = genericShrink
