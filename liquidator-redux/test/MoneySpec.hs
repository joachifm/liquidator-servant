{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MoneySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Money

spec :: Spec
spec = do
  describe "moneyToReal" $ do
    it "converts a Money value to a float" $ do
      moneyToReal (moneyFromAmount 1 25) == 1.25

  describe "ppMoney" $ do
    it "pretty-prints a Money value" $ do
      ppMoney (moneyFromAmount 1 25) == "1.25"

  describe "parseMoney" $ do
    it "reads a Money value" $ do
      parseMoney "1.25" == Right (moneyFromAmount 1 25)

    prop "is the inverse of ppMoney" $ \(x::Money) ->
      parseMoney (ppMoney x) == Right x

instance Arbitrary Money where
  arbitrary = moneyFromAmount
    <$> elements [0 .. 999999]
    <*> elements [0 .. 999999]
