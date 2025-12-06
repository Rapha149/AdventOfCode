module Years.Year2025.Day06 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra

getGrandTotal :: ([String] -> (Char, [String])) -> [String] -> Int
getGrandTotal f = sumOn' (solve . f) . split (all (== ' ')) . transpose
    where solve :: (Char, [String]) -> Int
          solve (op, xs) = (case op of
                                 '+' -> sum
                                 '*' -> product
                                 _ -> error "Unknown operation") $ map read xs

part1 :: Solution
part1 = V . getGrandTotal ((hd . lst &&& ini) . transpose)

part2 :: Solution
part2 = V . getGrandTotal (lst . hd &&& map ini)
