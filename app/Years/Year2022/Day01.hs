module Years.Year2022.Day01 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Ord

part1 :: Solution
part1 = V . maximum . map (sumOn' read) . split null

part2 :: Solution
part2 = V . sum . take 3 . sortOn Down . map (sumOn' read) . split null
