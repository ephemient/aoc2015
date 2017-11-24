module Day14Spec (spec) where

import Day14 (day14a, day14b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $
            day14a (unlines
              [ "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
              , "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
              ]) `shouldBe` Right 2660
