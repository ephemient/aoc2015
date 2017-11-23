module Day2Spec (spec) where

import Day2 (day2a, day2b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day2a "2x3x4" `shouldBe` Right 58
            day2a "1x1x10" `shouldBe` Right 43
    describe "part 2" $
        it "examples" $ do
            day2b "2x3x4" `shouldBe` Right 34
            day2b "1x1x10" `shouldBe` Right 14
