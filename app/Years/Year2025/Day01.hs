module Years.Year2025.Day01 (part1, part2) where

import Util.Util

parseInput :: [String] -> [Int]
parseInput [] = []
parseInput (('L':n):xs) = (-read n) : parseInput xs
parseInput (('R':n):xs) = read n : parseInput xs
parseInput _ = error "Invalid input."

part1 :: Solution
part1 = V . length . filter (== 0) . scanl ((`mod` 100) .: (+)) 50 . parseInput


getZeroCounts :: Int -> [Int] -> Int
getZeroCounts _ [] = 0
getZeroCounts dial (0:xs) = getZeroCounts dial xs
getZeroCounts dial (x:xs) = getZeroCounts dial' (x - signum x:xs) + if dial' == 0 then 1 else 0
    where dial' = (dial + signum x) `mod` 100

part2 :: Solution
part2 = V . getZeroCounts 50 . parseInput
