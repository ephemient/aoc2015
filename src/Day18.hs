module Day18 (day18a, day18b, parse, step, stick) where

parse :: String -> [[Bool]]
parse = map (map (== '#')) . lines

step :: [[Bool]] -> [[Bool]]
step grid = zipWith (zipWith next) grid $ neighbors grid where
    up, down, left, right :: [[Bool]] -> [[Bool]]
    up grid = tail grid ++ [repeat False]
    down grid = repeat False : grid
    left grid = [tail line ++ [False] | line <- grid]
    right = map (False:)
    accum :: [[Int]] -> [[Int]] -> [[Int]]
    accum = zipWith $ zipWith (+)
    neighbors :: [[Bool]] -> [[Int]]
    neighbors grid = foldr1 accum $ map (map $ map fromEnum)
      [ up grid, up $ right grid, right grid, down $ right grid
      , down grid, down $ left grid, left grid, up $ left grid
      ]
    next :: Bool -> Int -> Bool
    next _ 3 = True
    next True 2 = True
    next _ _ = False

stick :: [[Bool]] -> [[Bool]]
stick = tweak (tweak $ const True) where
    tweak f (x:xs) = f x : tweak' f xs
    tweak' f [x] = [f x]
    tweak' f (x:xs) = x : tweak' f xs

day18a :: String -> Int
day18a = length . concatMap (filter id) . (!! 100) . iterate step . parse

day18b :: String -> Int
day18b = length . concatMap (filter id) . (!! 100) .
         iterate (stick . step) . stick . parse
