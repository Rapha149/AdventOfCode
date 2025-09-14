module Year2020.Day06 (part1, part2) where

import Util
import Data.List.Extra

part1 :: Solution
part1 = V . sumOn' (length . nub . concat) . split null

part2 :: Solution
part2 = V . sumOn' (length . foldr1 intersect) . split null
