module Day4Spec (spec) where

import Day4 (day4a, day4b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $ do
            day4a "abcdef" `shouldBe` Just 609043
            day4a "pqrstuv" `shouldBe` Just 1048970
