module Years.Year2023.Day09 (part1, part2) where

import Util.Util

extrapolate :: ([Int] -> Int) -> (Int -> Int -> Int) -> [Int] -> Int
extrapolate pick add nums | all (== 0) nums = 0
                          | otherwise = pick nums `add` extrapolate pick add (differences nums)
    where differences :: [Int] -> [Int]
          differences [_] = []
          differences (a:b:xs) = b - a : differences (b:xs)
          differences [] = error "Empty list."

sumExtrapolated :: ([Int] -> Int) -> (Int -> Int -> Int) -> [String] -> Int
sumExtrapolated pick add = sum . map (extrapolate pick add . map read . words)

part1 :: Solution
part1 = V . sumExtrapolated lst (+)

part2 :: Solution
part2 = V . sumExtrapolated hd (-)
