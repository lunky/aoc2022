module Day8Spec where

import Day8
import Test.Hspec

spec :: Spec
spec = do
  describe "Day8" $ do
    it "should do sample 1" $ do
      let expected=21
      day8 _input `shouldBe` expected
  describe "Day8b" $ do
    it "should do sample 1" $ do
      let expected=8
      day8b _input `shouldBe` expected
