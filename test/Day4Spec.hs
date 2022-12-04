module Day4Spec where

import Day4
import Test.Hspec

spec :: Spec
spec = do
  describe "Day4" $ do
    it "should do sample 1" $ do
      let expected=2
      day4 _input `shouldBe` expected
  describe "Day4b" $ do
    it "should do sample 1" $ do
      let expected=4
      day4b _input `shouldBe` expected
