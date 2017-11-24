module Day5Spec (spec) where

import Day5 (day5a, day5b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day5a "ugknbfddgicrmopn" `shouldBe` 1
            day5a "aaa" `shouldBe` 1
            day5a "jchzalrnumimnmhp" `shouldBe` 0
            day5a "haegwjzuvuyypxyu" `shouldBe` 0
            day5a "dvszwmarrgswjxmb" `shouldBe` 0
    describe "part 2" $
        it "examples" $ do
            day5b "qjhvhtzxzqqjkmpb" `shouldBe` 1
            day5b "xxyxx" `shouldBe` 1
            day5b "uurcxstgmygtbstg" `shouldBe` 0
            day5b "ieodomkazucvgmuy" `shouldBe` 0
