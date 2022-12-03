module Day2Spec where

import Day2
import Test.Hspec

spec :: Spec
spec = do
  describe "Day2" $ do
    it "should do sample 1" $ do
      let expected = 15
      day2 _input `shouldBe` expected
    -- it "should do sample 2" $ do
    --   context <- readFile "data/day2.txt"
    --   let expected = 13009
    --   day2 context `shouldBe` expected
  describe "Day2b" $ do
    it "should do sample 1" $ do
      let expected = 12
      day2b _input `shouldBe` expected
    -- it "should do sample 2" $ do
    --   context <- readFile "data/day2.txt"
    --   let expected = 10398
    --   day2b context `shouldBe` expected
