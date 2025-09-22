module Years.Year2023.Day11 (part1, part2) where

import Util.Util
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

expand :: (Vec -> Int) -> Vec -> Int -> Map Vec Vec -> Map Vec Vec
expand _ _ (-1) galaxies = galaxies
expand pick add i galaxies = expand pick add (i - 1) $ (if Map.null $ Map.filterWithKey (\k _ -> pick k == i) galaxies
                                                           then Map.mapWithKey (\k v -> if pick k > i then onBoth (+) add v else v)
                                                           else id) galaxies

sumShortestPaths :: Int -> [String] -> Int
sumShortestPaths factor input = let galaxies = Map.fromList [((r, c), (0, 0)) | (r, row) <- zip [0..] input, (c, '#') <- zip [0..] row]
                                    expanded = Map.elems $ Map.mapWithKey (onBoth (+)) $ expand fst (factor - 1, 0) (length input) $ expand snd (0, factor - 1) (length $ hd input) galaxies
                                in sum [abs (r1 - r2) + abs (c1 - c2) | ((r1,c1):xs) <- tails expanded, (r2,c2) <- xs]

part1 :: Solution
part1 = V . sumShortestPaths 2

part2 :: Solution
part2 = V . sumShortestPaths 1000000
