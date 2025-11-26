module Years.Year2017.Day02 (part1, part2) where

import Util.Util
import Data.Ord
import Data.List.Extra

difference :: [Int] -> Int
difference xs = maximum xs - minimum xs

part1 :: Solution
part1 = V . sumOn' (difference . map read . words)

part2 :: Solution
part2 = V . sum . concatMap (\line -> [x `div` y | (x:ys) <- tails $ sortOn Down $ map read $ words line, y <- ys, x `mod` y == 0])
