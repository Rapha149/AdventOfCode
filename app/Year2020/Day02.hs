module Year2020.Day02 (part1, part2) where

import Util
import Data.List.Split

parseInput :: [String] -> [(Int, Int, Char, String)]
parseInput [] = []
parseInput (l:ls) = (n1, n2, lst policy, password) : parseInput ls
    where (policy, password) = pair $ splitOn ": " l
          (n1, n2) = pair $ map read $ splitOn "-" $ ini policy

isValid1 :: (Int, Int, Char, String) -> Bool
isValid1 (minCount, maxCount, char, password) = count >= minCount && count <= maxCount
    where count = length $ filter (== char) password

part1 :: Solution
part1 = V . length . filter isValid1 . parseInput


isValid2 :: (Int, Int, Char, String) -> Bool
isValid2 (i1, i2, char, password) = (password !! (i1 - 1) == char) /= (password !! (i2 - 1) == char)

part2 :: Solution
part2 = V . length . filter isValid2 . parseInput
