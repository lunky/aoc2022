module Day18Spec where

import Day18
import Test.Hspec

spec :: Spec
spec = do
  describe "Day18" $ do
    xit "should do sample 1" $ do
      let expected = 64
      day18 _input `shouldBe` expected
  describe "Day18b" $ do
    xit "should do sample 1" $ do
      let expected = 58
      day18b _input `shouldBe` expected
