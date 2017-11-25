module Day18Spec (spec) where

import Day18 (parse, step, stick)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $
        it "examples" $
            take 4 (tail $ iterate step start) `shouldBe` map (parse . unlines)
              [ [ "..##.."
                , "..##.#"
                , "...##."
                , "......"
                , "#....."
                , "#.##.."
                ]
              , [ "..###."
                , "......"
                , "..###."
                , "......"
                , ".#...."
                , ".#...."
                ]
              , [ "...#.."
                , "......"
                , "...#.."
                , "..##.."
                , "......"
                , "......"
                ]
              , [ "......"
                , "......"
                , "..##.."
                , "..##.."
                , "......"
                , "......"
                ]
              ]
    describe "part 2" $
        it "examples" $
            take 5 (tail . iterate (stick . step) $ stick start) `shouldBe` map (parse . unlines)
              [ [ "#.##.#"
                , "####.#"
                , "...##."
                , "......"
                , "#...#."
                , "#.####"
                ]
              , [ "#..#.#"
                , "#....#"
                , ".#.##."
                , "...##."
                , ".#..##"
                , "##.###"
                ]
              , [ "#...##"
                , "####.#"
                , "..##.#"
                , "......"
                , "##...."
                , "####.#"
                ]
              , [ "#.####"
                , "#....#"
                , "...#.."
                , ".##..."
                , "#....."
                , "#.#..#"
                ]
              , [ "##.###"
                , ".##..#"
                , ".##..."
                , ".##..."
                , "#.#..."
                , "##...#"
                ]
              ]
  where
    start = parse $ unlines
              [ ".#.#.#"
              , "...##."
              , "#....#"
              , "..#..."
              , "#.#..#"
              , "####.."
              ]
