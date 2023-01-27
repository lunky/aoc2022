module Day20Spec where

import Day20
import Test.Hspec

spec :: Spec
spec = do
  describe "Day20" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day20 _input `shouldBe` expected
  describe "Day20b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day20b _input `shouldBe` expected
