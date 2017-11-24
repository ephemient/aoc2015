module Day11Spec (spec) where

import Day11 (nextOk)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            nextOk "abcdefgh" `shouldBe` "abcdffaa"
            nextOk "ghijklmn" `shouldBe` "ghjaabcc"
