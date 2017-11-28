module Day19Spec (spec) where

import Day19 (day19a)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "part 1" $
        it "examples" $
            day19a (unlines
              [ "H => HO"
              , "H => OH"
              , "O => HH"
              , ""
              , "HOH"
              ]) `shouldBe` Right 4
