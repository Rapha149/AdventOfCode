module Year2024.Day01 (part1, part2) where

import Util
import Data.List
import Data.Tuple.Extra

part1 :: Solution
part1 = V . sum . map (abs . uncurry (-)) . uncurry zip . both sort . unzip . map ((\xs -> (hd xs, lst xs)) . map read . words)

part2 :: Solution
part2 = V . sum . (\(as, bs) -> map (\a -> a * (length . filter (== a)) bs) as) . unzip . map ((\xs -> (hd xs, lst xs)) . map read . words)
