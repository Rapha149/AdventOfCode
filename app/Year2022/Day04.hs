module Year2022.Day04 (part1, part2) where

import Util
import Data.Bifunctor
import Data.List
import Data.List.Split

parseInput :: ((Int, Int) -> Int) -> [String] -> [((Int, Int), (Int, Int))]
parseInput fSort = map (tuple . sortOn fSort . map (tuple . map read . splitOn "-") . splitOn ",")

part1 :: Solution
part1 = V . length . filter (\((a, b), (c, d)) -> a >= c && b <= d) . parseInput (uncurry subtract)

part2 :: Solution
part2 = V . length . filter (uncurry (>=)) . map (bimap snd fst) . parseInput fst
