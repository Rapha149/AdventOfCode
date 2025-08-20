module Year2022.Day01 (part1, part2) where

import Util
import Data.List
import Data.List.Split
import Data.Ord

part1 :: Solution
part1 = V . maximum . map (foldr ((+) . read) 0) . splitOn [""]

part2 :: Solution
part2 = V . sum . take 3 . sortOn Down . map (foldr ((+) . read) 0) . splitOn [""]
