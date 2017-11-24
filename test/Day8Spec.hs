module Day8Spec (spec) where

import Day8 (day8a, day8b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day8a "\"\"" `shouldBe` 2
            day8a "\"abc\"" `shouldBe` 2
            day8a "\"aaa\\\"aaa\"" `shouldBe` 3
            day8a "\"\\x27\"" `shouldBe` 5
    describe "part 2" $
        it "examples" $ do
            day8b "\"\"" `shouldBe` 4
            day8b "\"abc\"" `shouldBe` 4
            day8b "\"aaa\\\"aaa\"" `shouldBe` 6
            day8b "\"\\x27\"" `shouldBe` 5
