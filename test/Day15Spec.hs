module Day15Spec (spec) where

import Day15 (optimalCookie)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $
            optimalCookie (const True) product
                ["capacity", "durability", "flavor", "texture"]
                (unlines
                  [ "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
                  , "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
                  ])
                `shouldBe` Right ([44, 56], [68, 80, 152, 76])
