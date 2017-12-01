module Day19Spec (spec) where

import Day19 (day19a, day19b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            day19a (unlines
              [ "H => HO"
              , "H => OH"
              , "O => HH"
              , ""
              , "HOH"
              ]) `shouldBe` Right 4
    describe "part 2" $ do
        it "examples" $
            day19b (unlines
              [ "e => H"
              , "e => O"
              , "H => HO"
              , "H => OH"
              , "O => HH"
              , ""
              , "HOH"
              ]) `shouldBe` Right (Just 3)
        it "examples" $
            day19b (unlines
              [ "e => H"
              , "e => O"
              , "H => HO"
              , "H => OH"
              , "O => HH"
              , ""
              , "HOHOHO"
              ]) `shouldBe` Right (Just 6)
        it "examples" $
            day19b (unlines
              [ "Al => ThF"
              , "Al => ThRnFAr"
              , "B => BCa"
              , "B => TiB"
              , "B => TiRnFAr"
              , "Ca => CaCa"
              , "Ca => PB"
              , "Ca => PRnFAr"
              , "Ca => SiRnFYFAr"
              , "Ca => SiRnMgAr"
              , "Ca => SiTh"
              , "F => CaF"
              , "F => PMg"
              , "F => SiAl"
              , "H => CRnAlAr"
              , "H => CRnFYFYFAr"
              , "H => CRnFYMgAr"
              , "H => CRnMgYFAr"
              , "H => HCa"
              , "H => NRnFYFAr"
              , "H => NRnMgAr"
              , "H => NTh"
              , "H => OB"
              , "H => ORnFAr"
              , "Mg => BF"
              , "Mg => TiMg"
              , "N => CRnFAr"
              , "N => HSi"
              , "O => CRnFYFAr"
              , "O => CRnMgAr"
              , "O => HP"
              , "O => NRnFAr"
              , "O => OTi"
              , "P => CaP"
              , "P => PTi"
              , "P => SiRnFAr"
              , "Si => CaSi"
              , "Th => ThCa"
              , "Ti => BP"
              , "Ti => TiTi"
              , "e => HF"
              , "e => NAl"
              , "e => OMg"
              , ""
              , "NRnCaCaCaFYCaSiAlArPTiBCaPTiMg"
              ]) `shouldBe` Right (Just 13)
