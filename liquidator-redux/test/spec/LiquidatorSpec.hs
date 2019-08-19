{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LiquidatorSpec (spec) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson

import Test.Hspec
import Test.Hspec.QuickCheck
--import Test.QuickCheck
--import Servant.QuickCheck

import Spec.Instances ()

import Liquidator.Types
--import Liquidator.Web.Api
--import Liquidator.Web.Server

spec :: Spec
spec = do
  miscSpec
--  apiBestPracticesSpec
  jsonDecSpec

miscSpec :: Spec
miscSpec = do
  describe "split/joinNotes" $ do
    it "splitNotes is the inverse of joinNotes" $ do
      let notes =  ["foo", "bar", "baz"]
      splitNotes (joinNotes notes) `shouldBe` notes

{-
apiBestPracticesSpec :: Spec
apiBestPracticesSpec = do
  describe "WebApi" $ do
    it "follows best practices" $ do

      withServantServer api (server <$> newHandle defaultConfig) $ \burl ->
        serverSatisfies api burl stdArgs
          (
            -- Best practices
                not500

            -- RFC compliance
            <%> honoursAcceptHeader
            <%> htmlIncludesDoctype
            -- TODO(joachifm) this this
            -- <%> notAllowedContainsAllowHeader
            <%> unauthorizedContainsWWWAuthenticate

            -- Optional
            <%> createContainsValidLocation
            <%> mempty
          )
-}

jsonDecSpec :: Spec
jsonDecSpec = do
  describe "JSON decoder for Transaction" $ do
    prop "is reversible" $ \(x::Transaction) -> prop_jsDec_rev x

prop_jsDec_rev :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
prop_jsDec_rev x = Aeson.decode' (Aeson.encode x) == Just x