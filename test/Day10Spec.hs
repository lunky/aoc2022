module Day10Spec where

import Day10
import Test.Hspec

spec :: Spec
spec = do
  describe "Day10" $ do
    it "should do sample 1" $ do
      let expected=13140
      day10 _input `shouldBe` expected
  describe "Day10b" $ do
    it "should do sample 1" $ do
      let expected="##  ##  ##  ##  ##  ##  ##  ##  ##  ##  \n###   ###   ###   ###   ###   ###   ### \n####    ####    ####    ####    ####    \n#####     #####     #####     #####     \n######      ######      ######      ####\n#######       #######       #######     \n"
      day10b _input `shouldBe` expected
