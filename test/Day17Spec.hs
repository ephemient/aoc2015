module Day17Spec (spec) where

import Day17 (day17a, day17b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day17a 25 (unlines $ map show [20, 15, 10, 5, 5]) `shouldBe` 4
    describe "part 2" $
        it "examples" $
            day17b 25 (unlines $ map show [20, 15, 10, 5, 5]) `shouldBe` 3
