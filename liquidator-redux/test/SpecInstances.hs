{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module SpecInstances where

import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Liquidator
import Money

------------------------------------------------------------------------
-- TransactionFormData
------------------------------------------------------------------------

instance Arbitrary TransactionFormData where
  arbitrary = TransactionFormData
    <$> arbitrary
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
  arbitrary = moneyFromAmount
    <$> elements [0 .. 999999]
    <*> elements [0 .. 999999]
  shrink = genericShrink
