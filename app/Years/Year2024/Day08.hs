module Years.Year2024.Day08 (part1, part2) where

import Util.Util
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

getAntinodes :: (Int -> Int -> Vec -> Vec -> [Vec]) -> [String] -> Int
getAntinodes antinodesForAntennas input = Set.size $ Set.fromList $ concatMap (uncurry (antinodesForAntennas height width)) combinations
    where antennas = Map.elems $ Map.fromListWith (++) [(x, [(r, c)]) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row, x /= '.']
          combinations = concatMap (\xs -> [(a, b) | a <- xs, b <- xs, a /= b]) antennas
          height = length input
          width = length $ hd input

part1 :: Solution
part1 = V . getAntinodes (\h w a b -> filter (inBounds0 h w) [onBoth (\c d -> 2 * c - d) a b])


addUntilEnd :: Int -> Int -> Vec -> Vec -> [Vec]
addUntilEnd height width pos delta | inBounds0 height width next = next : addUntilEnd height width next delta
                                   | otherwise = []
    where next = onBoth (+) pos delta

part2 :: Solution
part2 = V . getAntinodes (\h w a b -> a : addUntilEnd h w a (onBoth (-) a b))
