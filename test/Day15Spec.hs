module Day15Spec where

import Day15
import Test.Hspec

spec :: Spec
spec = do
  describe "Day15" $ do
    it "should do sample 1" $ do
      let expected = 26
      day15 10 _input `shouldBe` expected
  describe "Day15b" $ do
    it "should do sample 1" $ do
      let expected = 56000011
      day15b 20 _input `shouldBe` expected
