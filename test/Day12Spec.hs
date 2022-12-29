module Day12Spec where

import Day12
import Test.Hspec

spec :: Spec
spec = do
  describe "Day12" $ do
    it "should do sample 1" $ do
      let expected=31
      day12 _input `shouldBe` expected
  describe "Day12b" $ do
    xit "should do sample 1" $ do
      let expected=0
      day12b _input `shouldBe` expected
