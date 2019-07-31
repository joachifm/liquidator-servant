module IORefSpec (spec) where

import Data.IORef
import IORef

import Test.Hspec

spec :: Spec
spec = do
  describe "postIncIORef" $ do
    it "returns the current value" $ do
      ref <- newIORef (1::Int)
      cur <- postIncIORef ref
      cur `shouldBe` 1

    it "increments the value by 1" $ do
      ref <- newIORef (1::Int)
      _ <- postIncIORef ref
      cur <- readIORef ref
      cur `shouldBe` 2
