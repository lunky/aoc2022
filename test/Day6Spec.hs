module Day6Spec where

import Day6
import Test.Hspec

spec :: Spec
spec = do
  describe "Day6" $ do
    it "should do sample 1" $ do
      let expected=7
      day6 _input `shouldBe` expected
    it "should do sample 2" $ do
      let expected=5
      day6 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` expected
    it "should do sample 3" $ do
      let expected=6
      day6 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` expected
    it "should do sample 3" $ do
      day6 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
    it "should do sample 3" $ do
      day6 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 11
  describe "Day6b" $ do
    it "should do sample 1" $ do
      let expected=19
      day6b _input `shouldBe` expected
    it "should do sample 2" $ do
      day6b "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 19
    it "should do sample 3" $ do
      day6b "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23
    it "should do sample 4" $ do
      day6b "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 23
    it "should do sample 5" $ do
      day6b "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29
    it "should do sample 6" $ do
      day6b "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 26
