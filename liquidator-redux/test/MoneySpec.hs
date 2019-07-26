{-# LANGUAGE OverloadedStrings #-}

module MoneySpec (spec) where

import Test.Hspec

import Money

spec :: Spec
spec = do
  describe "moneyToReal" $ do
    it "converts a Money value to a float" $ do
      moneyToReal (moneyFromAmount 125) == 1.25

  describe "ppMoney" $ do
    it "pretty-prints a Money value" $ do
      ppMoney (moneyFromAmount 125) == "1.25"

  describe "parseMoney" $ do
    it "reads a Money value" $ do
      parseMoney "1.25" == Right (moneyFromAmount 125)
