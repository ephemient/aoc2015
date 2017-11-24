module Day10Spec (spec) where

import Day10 (day10a, day10b, ls)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $ do
            ls "1" `shouldBe` "11"
            ls "11" `shouldBe` "21"
            ls "21" `shouldBe` "1211"
            ls "1211" `shouldBe` "111221"
            ls "111221" `shouldBe` "312211"
