module Years.Year2017.Day04 (part1, part2) where

import Util.Util
import Data.List

isValid :: [String] -> Bool
isValid xs = and [x /= y | (x:ys) <- tails xs, y <- ys]

part1 :: Solution
part1 = V . length . filter (isValid . words)

part2 :: Solution
part2 = V . length . filter (isValid . map sort . words)
