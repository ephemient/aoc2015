module Day1Spec (spec) where

import Day1 (day1a, day1b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day1a "(())" `shouldBe` 0
            day1a "()()" `shouldBe` 0
            day1a "(((" `shouldBe` 3
            day1a "(()(()(" `shouldBe` 3
            day1a "))(((((" `shouldBe` 3
            day1a "())" `shouldBe` -1
            day1a "))(" `shouldBe` -1
            day1a ")))" `shouldBe` -3
            day1a ")())())" `shouldBe` -3
    describe "part 2" $
        it "examples" $ do
            day1b ")" `shouldBe` Just 1
            day1b "()())" `shouldBe` Just 5
