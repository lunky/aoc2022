module Day11Spec where

import Day11
import Test.Hspec

spec :: Spec
spec = do
  describe "Day11" $ do
    it "should do sample 1" $ do
      let expected=10605
      day11 _input `shouldBe` expected
  describe "Day11b" $ do
    it "should do sample 1" $ do
      let expected=2713310158
      day11b _input 10000 `shouldBe` expected
    it "should do sample 2" $ do
      let expected=27019168
      day11b _input 1000 `shouldBe` expected
    it "should do sample 1 20 rounds" $ do
      let expected=99*103
      day11b _input 20 `shouldBe` expected
