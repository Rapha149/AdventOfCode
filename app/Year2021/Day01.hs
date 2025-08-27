module Year2021.Day01 (part1, part2) where

import Util
import Data.List

part1 :: Solution
part1 input = let numbers = map read input :: [Int]
              in V $ length $ filter (uncurry (<)) $ zip numbers (tl numbers)

part2 :: Solution
part2 input = let numbers = map read input :: [Int]
                  windows = map sum $ filter ((== 3) . length) $ map (take 3) $ tails numbers
              in V $ length $ filter (uncurry (<)) $ zip windows (tl windows)
