module Years.Year2023.Day06 (part1, part2) where

import Util.Util

numberOfWaysToWin :: (Int, Int) -> Int
numberOfWaysToWin (time, distance) = length $ filter (\i -> (time - i) * i > distance) [1..time - 1]

part1 :: Solution
part1 = V . product . map numberOfWaysToWin . uncurry zip . pair . map (map read . drop 1 . words)

part2 :: Solution
part2 input = V $ length $ filter (\i -> (time - i) * i > distance) [1..time - 1]
    where (time, distance) = pair $ map (read . concat . drop 1 . words) input :: (Int, Int)
