module Years.Year2019.Day16 (part1, part2) where

import Util.Util
import Data.Char

phase1 :: [[Int]] -> [Int] -> [Int]
phase1 patterns list = map ((`mod` 10) . abs . sum . zipWith (*) list) patterns

parse :: [Int] -> Int
parse = foldl (\acc v -> acc * 10 + v) 0

part1 :: Solution
part1 input = V $ parse $ take 8 $ foldr ($) list $ replicate 100 (phase1 patterns)
    where list = map digitToInt $ hd input
          patterns = [tl $ cycle $ concatMap (replicate i) [0, 1, 0, -1] | i <- [1..length list]]


phase2 :: [Int] -> [Int]
phase2 list = buildList list $ sum list
    where buildList :: [Int] -> Int -> [Int]
          buildList [] total = [total `mod` 10]
          buildList (x:xs) total = total `mod` 10 : buildList xs (total - x)

part2 :: Solution
part2 input = V $ parse $ take 8 $ foldr ($) (drop pos list) $ replicate 100 phase2
    where list = concat $ replicate 10000 $ map digitToInt $ hd input
          pos = parse $ take 7 list
