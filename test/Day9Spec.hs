module Day9Spec (spec) where

import Day9 (day9a, day9b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $ do
            day9a (unlines
              [ "London to Dublin = 464"
              , "London to Belfast = 518"
              , "Dublin to Belfast = 141"
              ]) `shouldBe` Just 605
    describe "part 2" $
        it "examples" $ do
            day9b (unlines
              [ "London to Dublin = 464"
              , "London to Belfast = 518"
              , "Dublin to Belfast = 141"
              ]) `shouldBe` Just 982
