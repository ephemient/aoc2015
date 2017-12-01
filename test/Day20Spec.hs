module Day20Spec (spec) where

import Day20 (day20a)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $ do
            day20a "10" `shouldBe` Just 1
            day20a "20" `shouldBe` Just 2
            day20a "40" `shouldBe` Just 3
            day20a "80" `shouldBe` Just 6
