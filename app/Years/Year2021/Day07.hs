module Years.Year2021.Day07 (part1, part2) where

import Util.Util
import Data.List.Extra

getMinFuel :: (Int -> Int) -> [String] -> Int
getMinFuel f input = minimum [sumOn' (f . abs . (p-)) numbers | p <- [minimum numbers..maximum numbers]]
    where numbers = map read $ splitOn "," $ hd input

part1 :: Solution
part1 = V . getMinFuel id

part2 :: Solution
part2 = V . getMinFuel (\x -> x * (x + 1) `div` 2)
