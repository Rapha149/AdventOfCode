module Year2020.Day03 (part1, part2) where

import Util
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> (Int, Int, Set Vec)
parseInput input = (length input, length $ hd input, Set.fromList [(r, c) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row, x == '#'])

countTrees :: Int -> Int -> Set Vec -> Vec -> Vec -> Int
countTrees height width trees (sr, sc) (r, c) | r >= height = 0
                                              | otherwise = fromEnum (Set.member nextPos trees) + countTrees height width trees (sr, sc) nextPos
    where nextPos = (r + sr, (c + sc) `mod` width)

part1 :: Solution
part1 input = let (height, width, trees) = parseInput input
              in V $ countTrees height width trees (1, 3) (0, 0)

part2 :: Solution
part2 input = let (height, width, trees) = parseInput input
                  count = countTrees height width trees
              in V $ productOn' (`count` (0, 0)) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
