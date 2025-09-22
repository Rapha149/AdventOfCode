module Years.Year2019.Day01 (part1, part2) where

import Util.Util
import Data.List.Extra

part1 :: Solution
part1 = V . sumOn' (subtract 2 . (`div` 3) . read)


getFuel :: Int -> Int
getFuel mass | fuel > 0 = fuel + getFuel fuel
             | otherwise = 0
    where fuel = mass `div` 3 - 2

part2 :: Solution
part2 = V . sumOn' (getFuel . read)
