{-# LANGUAGE TypeApplications #-}

module Liquidator.SchemaSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC

import qualified Data.Aeson as Aeson

import Liquidator.Schema

spec :: Spec
spec = do
  describe "Balance" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @Balance

  describe "BankBalance" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @BankBalance

  describe "Company" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @Company

  describe "Month" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @Month

  describe "Pagination" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @Pagination

  describe "RecurringTransaction" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @RecurringTransaction

  describe "Role" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @Role

  describe "Transaction" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @Transaction

  describe "TransactionType" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @TransactionType

  describe "User" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @User

  describe "UserCreate" $ do
    prop "from/toJSON is reversible" $
      prop_fromToJsonRev @UserCreate

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

prop_fromToJsonRev :: (Aeson.FromJSON e, Aeson.ToJSON e, Eq e) => e -> Bool
prop_fromToJsonRev x = Aeson.decode (Aeson.encode x) == Just x
