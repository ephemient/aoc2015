module Day6Spec (spec) where

import Day6 (day6a, day6b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day6a "turn on 0,0 through 999,999" `shouldBe` Right 1000000
            day6a "toggle 0,0 through 999,0" `shouldBe` Right 1000
            day6a "toggle 499,499 through 500,500" `shouldBe` Right 4
