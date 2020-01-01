module Day25Spec (spec) where

import Day25 (day25)
import Test.Hspec (Spec, it, shouldBe)

text :: Int -> Int -> String
text x y = "To continue, please consult the code grid in the manual.  " ++
    "Enter the code at row " ++ show x ++ ", column " ++ show y ++ ".\n"

spec :: Spec
spec = it "examples" $ do
    day25 (text 1 1) `shouldBe` 20151125
    day25 (text 1 2) `shouldBe` 18749137
    day25 (text 1 3) `shouldBe` 17289845
    day25 (text 1 4) `shouldBe` 30943339
    day25 (text 1 5) `shouldBe` 10071777
    day25 (text 1 6) `shouldBe` 33511524
    day25 (text 2 1) `shouldBe` 31916031
    day25 (text 2 2) `shouldBe` 21629792
    day25 (text 2 3) `shouldBe` 16929656
    day25 (text 2 4) `shouldBe` 7726640
    day25 (text 2 5) `shouldBe` 15514188
    day25 (text 2 6) `shouldBe` 4041754
    day25 (text 3 1) `shouldBe` 16080970
    day25 (text 3 2) `shouldBe` 8057251
    day25 (text 3 3) `shouldBe` 1601130
    day25 (text 3 4) `shouldBe` 7981243
    day25 (text 3 5) `shouldBe` 11661866
    day25 (text 3 6) `shouldBe` 16474243
    day25 (text 4 1) `shouldBe` 24592653
    day25 (text 4 2) `shouldBe` 32451966
    day25 (text 4 3) `shouldBe` 21345942
    day25 (text 4 4) `shouldBe` 9380097
    day25 (text 4 5) `shouldBe` 10600672
    day25 (text 4 6) `shouldBe` 31527494
    day25 (text 5 1) `shouldBe` 77061
    day25 (text 5 2) `shouldBe` 17552253
    day25 (text 5 3) `shouldBe` 28094349
    day25 (text 5 4) `shouldBe` 6899651
    day25 (text 5 5) `shouldBe` 9250759
    day25 (text 5 6) `shouldBe` 31663883
    day25 (text 6 1) `shouldBe` 33071741
    day25 (text 6 2) `shouldBe` 6796745
    day25 (text 6 3) `shouldBe` 25397450
    day25 (text 6 4) `shouldBe` 24659492
    day25 (text 6 5) `shouldBe` 1534922
    day25 (text 6 6) `shouldBe` 27995004
