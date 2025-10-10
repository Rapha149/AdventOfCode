module Years.Year2020.Day10 (part1, part2) where

import Util.Util
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

part1 :: Solution
part1 input = V $ count 1 diffs * count 3 diffs
    where xs :: [Int] = sort $ map read input
          diffs = 3 : zipWith (-) xs (0:xs)
          count i = length . filter (== i)


getCombinations :: [Int] -> Map Int Int -> Int
getCombinations [] _ = error "Empty list."
getCombinations [a] counts = counts Map.! a
getCombinations (a:xs) counts = getCombinations xs $ Map.unionWith (+) counts $ Map.fromList $ map (, count) reachable
    where count = counts Map.! a
          reachable = filter (<= a + 3) $ take 3 xs

part2 :: Solution
part2 input = V $ getCombinations (0:xs) (Map.singleton 0 1)
    where xs :: [Int] = sort $ map read input
