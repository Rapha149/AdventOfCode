module Years.Year2025.Day07 (part1, part2) where

import Util.Util
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> (Int, Set Vec)
parseInput input = (startX, Map.keysSet $ Map.filter (== '^') grid)
    where grid = Map.fromList [((x, y), c) | (y, row) <- zip [0..] input, (x, c) <- zip [0..] row]
          ((startX, _), _) = Map.findMin $ Map.filter (== 'S') grid

countSplits :: Set Vec -> Int -> Int -> [Int] -> Int
countSplits splitters maxY y xs | y > maxY = 0
                                | otherwise = splits + countSplits splitters maxY (y + 1) (nub xs')
    where xs' = concatMap (\x -> if (x, y) `Set.member` splitters then [x - 1, x + 1] else [x]) xs
          splits = length xs' - length xs

part1 :: Solution
part1 input = V $ countSplits splitters maxY minY [x]
    where (x, splitters) = parseInput input
          (_, (minY, maxY)) = getBounds $ Set.toList splitters


countPaths :: Set Vec -> Int -> Vec -> Map Vec Int -> (Int, Map Vec Int)
countPaths splitters maxY pos@(x, y) seen | y > maxY = (1, seen)
                                          | pos `Map.member` seen = (seen Map.! pos, seen)
                                          | pos `Set.notMember` splitters = let (count, seen') = countPaths splitters maxY (x, y + 1) seen
                                                                            in (count, Map.insert pos count seen')
                                          | otherwise = (lCount + rCount, Map.insert pos (lCount + rCount) rSeen)
    where (lCount, lSeen) = countPaths splitters maxY (x - 1, y) seen
          (rCount, rSeen) = countPaths splitters maxY (x + 1, y) lSeen

part2 :: Solution
part2 input = V $ fst $ countPaths splitters maxY (x, minY) Map.empty
    where (x, splitters) = parseInput input
          (_, (minY, maxY)) = getBounds $ Set.toList splitters
