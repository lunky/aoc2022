module Day10Spec where

import Day10
import Test.Hspec

spec :: Spec
spec = do
  describe "Day10" $ do
    xit "should do sample 1" $ do
      let expected=0
      day10 _input `shouldBe` expected
  describe "Day10b" $ do
    xit "should do sample 1" $ do
      let expected=0
      day10b _input `shouldBe` expected