module Day1Spec where
import Day1 
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "test harness" $ do
    it "should demonstrate that test are working" $ do
      1 `shouldBe` 1
  describe "day1" $ do
    it "should work for pattern 1" $ do
      let input = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n";
      let expected = 24000
      day1 input `shouldBe` expected
  describe "day1b" $ do
    it "should work for pattern 1" $ do
      let input = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n";
      let expected = 45000
      day1b input `shouldBe` expected

