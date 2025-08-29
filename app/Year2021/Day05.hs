module Year2021.Day05 (part1, part2) where

import Util
import Data.List.Split
import Data.Tuple.Extra
import qualified Data.Map.Strict as Map

parseInput :: [String] -> [(Vec, Vec)]
parseInput = map (both (tuple . map read . splitOn ",") . (\xs -> (hd xs, lst xs)) . words)

getStraightLines :: [(Vec, Vec)] -> [Vec]
getStraightLines ls = [(x, y) | ((x1, y1), (x2, y2)) <- ls, x1 == x2 || y1 == y2, x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]

getOverlapping :: [Vec] -> Int
getOverlapping = Map.size . Map.filter (> 1) . Map.fromListWith (+) . map (, 1 :: Int)

part1 :: Solution
part1 = V . getOverlapping . getStraightLines . parseInput

part2 :: Solution
part2 input = let ls = parseInput input
                  diagonal = [(x1 + n * dx, y1 + n * dy) | ((x1, y1), (x2, y2)) <- ls, x1 /= x2 && y1 /= y2,
                                                           let dx = signum $ x2 - x1, let dy = signum $ y2 - y1, n <- [0..abs $ x1 - x2]]
              in V $ getOverlapping $ diagonal ++ getStraightLines ls
