module Year2021.Day11 (part1, part2) where

import Util
import Data.Char
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> Map Vec Int
parseInput input = Map.fromList [((r, c), digitToInt x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]

step :: Map Vec Int -> Map Vec Int
step levels = flash $ Map.map (+1) levels
    where flash :: Map Vec Int -> Map Vec Int
          flash levels' = let flashed = Map.keys $ Map.filter (> 9) levels'
                              flashedSet = Set.fromList flashed
                              increased = foldr (Map.adjust (+1)) levels' [(r + dr, c + dc) | (r, c) <- flashed, dr <- [-1..1], dc <- [-1..1], dr /= 0 || dc /= 0]
                          in if null flashed then levels' else Map.union (Map.fromSet (const 0) flashedSet) (flash $ Map.withoutKeys increased flashedSet)

countFlashes :: Int -> Map Vec Int -> Int
countFlashes 0 _ = 0
countFlashes n levels = Map.size (Map.filter (== 0) levels') + countFlashes (n - 1) levels'
    where levels' = step levels

part1 :: Solution
part1 = V . countFlashes 100 . parseInput


synchronize :: Int -> Map Vec Int -> Int
synchronize n levels | Map.null $ Map.filter (> 0) levels = n
                     | otherwise = synchronize (n + 1) $ step levels

part2 :: Solution
part2 = V . synchronize 0 . parseInput
