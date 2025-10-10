module Years.Year2021.Day01 (part1, part2) where

import Util.Util
import Data.List

part1 :: Solution
part1 input = V $ length $ filter (uncurry (<)) $ zip numbers (tl numbers)
    where numbers = map read input :: [Int]

part2 :: Solution
part2 input = V $ length $ filter (uncurry (<)) $ zip windows (tl windows)
    where numbers = map read input :: [Int]
          windows = map sum $ filter ((== 3) . length) $ map (take 3) $ tails numbers
