module Day21Spec where

import Day21
import Test.Hspec

spec :: Spec
spec = do
  describe "Day21" $ do
    xit "should do sample 1" $ do
      let expected = 152
      day21 _input `shouldBe` expected
  describe "Day21b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day21b _input `shouldBe` expected
