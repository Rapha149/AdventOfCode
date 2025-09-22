module Years.Year2022.Day04 (part1, part2) where

import Util.Util
import Data.Bifunctor
import Data.List
import Data.List.Split

parseInput :: (Vec -> Int) -> [String] -> [(Vec, Vec)]
parseInput fSort = map (pair . sortOn fSort . map (pair . map read . splitOn "-") . splitOn ",")

part1 :: Solution
part1 = V . length . filter (\((a, b), (c, d)) -> a >= c && b <= d) . parseInput (uncurry subtract)

part2 :: Solution
part2 = V . length . filter (uncurry (>=)) . map (bimap snd fst) . parseInput fst
