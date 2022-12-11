module Day9Spec where

import Day9 
import Test.Hspec

spec :: Spec
spec = do
  describe "Day9" $ do
    it "should do sample 1" $ do
      let _input="R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
      let expected = 13
      day9 _input `shouldBe` expected
  describe "Day9b" $ do
    it "should do sample 1" $ do
      let _input="R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
      let expected=1
      day9b _input `shouldBe` expected
    it "should do sample 2" $ do
      let _input2="R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"
      let expected=36
      day9b _input2 `shouldBe` expected
  describe "Coordinate movements" $ do
    it "should go left" $ do
      left (C 0 0) `shouldBe` C 0 (-1)
    it "should go upLeft" $ do
      upLeft (C 0 0) `shouldBe` C (-1) (-1)
    it "should go up " $ do
      left (C 0 0) `shouldBe` C 0 (-1)
    it "should go upRight" $ do
      upLeft (C 0 0) `shouldBe` (C (-1) 1)
