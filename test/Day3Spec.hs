module Day3Spec (spec) where

import Day3 (day3a, day3b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day3a ">" `shouldBe` 2
            day3a "^>v<" `shouldBe` 4
            day3a "^v^v^v^v^v" `shouldBe` 2
    describe "part 2" $
        it "examples" $ do
            day3b "^v" `shouldBe` 3
            day3b "^>v<" `shouldBe` 3
            day3b "^v^v^v^v^v" `shouldBe` 11
