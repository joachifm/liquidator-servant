{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MoneySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Money
import Spec.Instances ()

spec :: Spec
spec = do
  describe "IsString Money" $ do
    it "correctly handles 2.0" $ do
      ("2.0" :: Money) == MkMoney 200

    -- TODO(joachifm) bug: "0.1" is MkMoney 1 but should be 10
    it "correctly handles 0.10" $ do
      ("0.10" :: Money) == MkMoney 10

  describe "moneyToReal" $ do
    it "converts a Money value to a float" $ do
      moneyToReal (moneyFromAmounts 1 25) == 1.25

  describe "moneyToAmounts" $ do
    it "converts a Money value back to pairs" $ do
      moneyToAmounts (moneyFromAmounts 1 25) == (1, 25)

    prop "is the inverse of moneyFromAmount" $ \(x::Money) ->
      uncurry moneyFromAmounts (moneyToAmounts x) == x

  describe "ppMoney" $ do
    it "pretty-prints a Money value" $ do
      ppMoney (moneyFromAmounts 1 25) == "1.25"

  describe "parseMoney" $ do
    it "reads a Money value" $ do
      parseMoney "1.25" == Right (moneyFromAmounts 1 25)

    prop "is the inverse of ppMoney" $ \(x::Money) ->
      parseMoney (ppMoney x) == Right x
