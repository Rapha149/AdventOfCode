module Years.Year2020.Day03 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> (Int, Int, Set Vec)
parseInput input = (length input, length $ hd input, Set.fromList [(r, c) | (r, row) <- zip [0..] input, (c, '#') <- zip [0..] row])

countTrees :: Int -> Int -> Set Vec -> Vec -> Vec -> Int
countTrees height width trees (sr, sc) (r, c) | r >= height = 0
                                              | otherwise = fromEnum (Set.member nextPos trees) + countTrees height width trees (sr, sc) nextPos
    where nextPos = (r + sr, (c + sc) `mod` width)

part1 :: Solution
part1 input = V $ countTrees height width trees (1, 3) (0, 0)
    where (height, width, trees) = parseInput input

part2 :: Solution
part2 input = V $ productOn' (`count` (0, 0)) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
    where (height, width, trees) = parseInput input
          count = countTrees height width trees
