module Day12Spec (spec) where

import Day12 (day12a, day12b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day12a "[1,2,3]" `shouldBe` Just 6
            day12a "{\"a\":2,\"b\":4}" `shouldBe` Just 6
            day12a "[[[3]]]" `shouldBe` Just 3
            day12a "{\"a\":{\"b\":4},\"c\":-1}" `shouldBe` Just 3
            day12a "{\"a\":[-1,1]}" `shouldBe` Just 0
            day12a "[-1,{\"a\":1}]" `shouldBe` Just 0
            day12a "[]" `shouldBe` Just 0
            day12a "{}" `shouldBe` Just 0
    describe "part 2" $
        it "examples" $ do
            day12b "[1,2,3]" `shouldBe` Just 6
            day12b "[1,{\"c\":\"red\",\"b\":2},3]" `shouldBe` Just 4
            day12b "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}" `shouldBe` Just 0
            day12b "[1,\"red\",5]" `shouldBe` Just 6
