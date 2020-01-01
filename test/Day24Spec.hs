module Day24Spec (spec) where

import Day24 (day24a, day24b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day24a (unlines $ show <$> [1..5] ++ [7..11]) `shouldBe` Just 99
    describe "part 2" $
        it "examples" $
            day24b (unlines $ show <$> [1..5] ++ [7..11]) `shouldBe` Just 44
