module Day17Spec where

import Day17
import Test.Hspec

spec :: Spec
spec = do
  describe "Day17" $ do
    it "should do sample 1" $ do
      let expected = 3068
      day17 _input `shouldBe` expected
  describe "Day17b" $ do
    it "should do sample 1" $ do
      let expected = 1514285714288
      let cycleStones = 35
      let leadStones = 15
      day17b cycleStones leadStones _input `shouldBe` expected
