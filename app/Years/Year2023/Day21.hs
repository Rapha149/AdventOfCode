module Years.Year2023.Day21 (part1, part2) where

import Util.Util
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> (Vec, Set Vec)
parseInput input = (fst $ fromJust $ find ((== 'S') . snd) grid, Set.fromList $ map fst $ filter ((/= '#') . snd) grid)
    where grid = [((r, c), x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]

pathLengths :: Set Vec -> [(Vec, Int)] -> Map Vec Int -> [Int]
pathLengths _ [] paths = Map.elems paths
pathLengths plots ((p, n):ps) paths = pathLengths plots (ps ++ adjacents) $ foldr (uncurry Map.insert) paths adjacents
    where adjacents = map (, n + 1) $ filter (\x -> Set.member x plots && Map.notMember x paths) $ map (onBoth (+) p) [(-1, 0), (1, 0), (0, -1), (0, 1)]

part1 :: Solution
part1 input = V $ length $ filter (\n -> n <= 64 && even n) lengths
    where (start, plots) = parseInput input
          lengths = pathLengths plots [(start, 0)] $ Map.singleton start 0

-- https://github.com/villuna/aoc23/wiki/A-Geometric-solution-to-advent-of-code-2023,-day-21
part2 :: Solution
part2 input = V $ (n + 1) * (n + 1) * oddAll + n * n * evenAll - (n + 1) * oddCorners + n * evenCorners
    where (start, plots) = parseInput input
          lengths = pathLengths plots [(start, 0)] $ Map.singleton start 0
          (evenAll, oddAll) = both length $ partition even lengths
          (evenCorners, oddCorners) = both length $ partition even $ filter (> 65) lengths
          height = length input
          n = (26501365 - height `div` 2) `div` height
