module Day13Spec where

import Day13
import Test.Hspec

spec :: Spec
spec = do
  describe "'inOrder" $ do
    it "left side is smaller" $ do
      inOrder' (p "[[1],[2,3,4]]", p "[[1],4]") `shouldBe` LT
      inOrder' (p "[1,1,3,1,1]", p"[1,1,5,1,1]") `shouldBe` LT
    it "right side is smaller" $ do
      inOrder' (p "[[1],4]", p "[[1],[2,3,4]]") `shouldBe` GT
      inOrder' (p "[1,1,5,1,1]",p "[1,1,3,1,1]") `shouldBe` GT
    it "mixed types rhs is smaller" $ do 
      inOrder' (p "[9]" , p "[[8,7,6]]") `shouldBe` GT
    it "mixed types lhs is smaller" $ do 
      inOrder' (p "[[8,7,6]]", p "[9]") `shouldBe` LT
    it "lhs runs out of items" $ do 
      inOrder' (p "[[4,4],4,4]", p "[[4,4],4,4,4]") `shouldBe` LT
    it "rhs runs out of items" $ do 
      inOrder' (p "[[4,4],4,4,4]", p "[[4,4],4,4]") `shouldBe` GT
    it "lhs runs out of items" $ do 
      inOrder' (p "[4,4,4,4]", p "[4,4,4,4,4]") `shouldBe` LT
    it "rhs runs out of items" $ do 
      inOrder' (p "[4,4,4,4,4]", p "[4,4,4,4]") `shouldBe` GT
    it "rhs is smaller " $ do 
      inOrder' (p "[1,[2,[3,[4,[5,6,7]]]],8,9]", p "[1,[2,[3,[4,[5,6,0]]]],8,9]") `shouldBe` GT
    it "lhs is smaller " $ do 
      inOrder' (p "[1,[2,[3,[4,[5,6,0]]]],8,9]", p "[1,[2,[3,[4,[5,6,7]]]],8,9]") `shouldBe` LT
    it "lhs is smaller 2" $ do 
      inOrder' (p "[[5],[4]]", p "[10]") `shouldBe` LT
    it "lhs is smaller 4" $ do 
      inOrder' (p "[[[5],[4]]]", p "[[10],[2]]") `shouldBe` LT
    it "lhs is smaller 4" $ do 
      inOrder' (p "[[[5],[4]]]", p "[[10],[]]") `shouldBe` LT
  describe "Day13" $ do
    it "should do sample 1" $ do
      let expected = 13
      day13 _input `shouldBe` expected
  describe "Day13b" $ do
    it "should do sample 1" $ do
      let expected = 140
      day13b _input `shouldBe` expected
