module Years.Year2025.Day04 (part1, part2) where

import Util.Util
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> Set Vec
parseInput input = Set.fromList [(r, c) | (r, row) <- zip [0..] input, (c, '@') <- zip [0..] row]

getAccessible :: Set Vec -> Set Vec
getAccessible rolls = Set.filter (\(r, c) -> length [True | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0), (r + dr, c + dc) `Set.member` rolls] < 4) rolls

part1 :: Solution
part1 = V . length . getAccessible . parseInput


countAccessible :: Set Vec -> Int
countAccessible rolls | null accessible = 0
                      | otherwise = length accessible + countAccessible (rolls `Set.difference` accessible)
    where accessible = getAccessible rolls

part2 :: Solution
part2 = V . countAccessible . parseInput
