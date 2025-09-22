module Years.Year2022.Day10 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra
import qualified Data.Set as Set

getCycleValues :: Int -> [String] -> [Int]
getCycleValues _ [] = []
getCycleValues x ("noop":is) = x : getCycleValues x is
getCycleValues x (i:is) = x : x : getCycleValues (x + read (drop 5 i)) is

part1 :: Solution
part1 = V . sumOn' (uncurry (*)) . filter (\(n, _) -> (n - 20) `mod` 40 == 0) . zip [1..] . getCycleValues 1

part2 :: Solution
part2 input = OCR $ Set.fromList [(x, y) | (n, sprite) <- zip [0..] $ getCycleValues 1 input, let (x, y) = ((`mod` 40) &&& (`div` 40)) n, abs (sprite - x) <= 1]
