module Years.Year2017.Day01 (part1, part2) where

import Util.Util
import Data.Char
import Data.List.Extra

part1 :: Solution
part1 input = V $ sumOn' fst $ filter (uncurry (==)) $ zip xs (tl xs ++ [hd xs])
    where xs = map digitToInt $ hd input

part2 :: Solution
part2 input = V $ sum $ zipWith (\i x -> if x == xs !! ((i + half) `mod` len) then x else 0) [0..] xs
    where xs = map digitToInt $ hd input
          len = length xs
          half = len `div` 2
