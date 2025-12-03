module Years.Year2025.Day03 (part1, part2) where

import Util.Util
import Data.Char
import Data.List.Extra

getMaxJoltage :: Int -> [Int] -> Int
getMaxJoltage 0 _ = 0
getMaxJoltage n xs = x * 10 ^ (n - 1) + getMaxJoltage (n - 1) (tl $ dropWhile (/= x) xs)
    where x = maximum $ dropEnd (n - 1) xs

part1 :: Solution
part1 = V . sumOn' (getMaxJoltage 2 . map digitToInt)

part2 :: Solution
part2 = V . sumOn' (getMaxJoltage 12 . map digitToInt)
