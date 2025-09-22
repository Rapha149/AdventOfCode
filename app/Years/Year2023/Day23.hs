module Years.Year2023.Day23 (part1, part2) where

import Util.Util
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> (Vec, Map Vec Vec)
parseInput input = ((length input - 1, length (hd input) - 2),
                    Map.fromList [((r, c), getAllowed (r, c) x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row, x /= '.'])
    where getAllowed :: Vec -> Char -> Vec
          getAllowed _ '#' = (-1, -1)
          getAllowed (r, c) '>' = (r, c - 1)
          getAllowed (r, c) '<' = (r, c + 1)
          getAllowed (r, c) 'v' = (r - 1, c)
          getAllowed (r, c) '^' = (r + 1, c)
          getAllowed _ _ = error "Invalid character."

bfs :: Vec -> Map Vec Vec -> [(Vec, Vec, Set Vec)] -> Int
bfs _ _ [] = 0
bfs end obstacles ((pos, prev, path):xs) | pos == end = max (Set.size path) (bfs end obstacles xs)
                                         | otherwise = bfs end obstacles (next ++ xs)
    where next = map (, pos, Set.insert pos path) $ filter (\p -> p /= prev && Set.notMember p path && maybe True (== pos) (obstacles Map.!? p)) $ map (onBoth (+) pos) [(-1, 0), (1, 0), (0, -1), (0, 1)]

part1 :: Solution
part1 input = let (end, obstacles) = parseInput input
              in V $ bfs end obstacles [((0, 1), (-1, 1), Set.empty)]


findJunctions :: Vec -> Vec -> Set Vec -> [(Vec, Vec, Vec, Int)] -> Map Vec [(Vec, Int)] -> (Int, Vec, Vec, Map Vec [(Vec, Int)])
findJunctions start end _ [] junctions = (dStart + dEnd, jStart, jEnd, Map.adjust (delete (start, dStart)) jStart $ Map.adjust (delete (end, dEnd)) jEnd junctions)
    where findSE :: Vec -> (Vec, Int)
          findSE pos = second (snd . fromJust . find ((==) pos . fst)) $ hd $ Map.toList $ Map.filter (any ((==) pos . fst)) junctions
          (jStart, dStart) = findSE start
          (jEnd, dEnd) = findSE end
findJunctions start end obstacles ((pos, prev, junc, dist):xs) junctions | pos == end = findJunctions start end obstacles xs $ Map.adjust ((end, dist):) junc junctions
                                                                         | length next == 1 = findJunctions start end obstacles ((hd next, pos, junc, dist + 1):xs) junctions
                                                                         | Map.member pos junctions = findJunctions start end obstacles xs $ Map.adjust ((pos, dist):) junc junctions
                                                                         | otherwise = findJunctions start end obstacles (map (, pos, pos, 1) next ++ xs) $
                                                                                       Map.adjust ((pos, dist):) junc $ Map.insert pos [(junc, dist)] junctions
    where next = filter (\p -> p /= prev && Set.notMember p obstacles) $ map (onBoth (+) pos) [(-1, 0), (1, 0), (0, -1), (0, 1)]

bfsJunctions :: Vec -> Map Vec [(Vec, Int)] -> [(Vec, Vec, Set Vec, Int)] -> Int
bfsJunctions _ _ [] = 0
bfsJunctions end junctions ((pos, prev, path, len):xs) | pos == end = max len $ bfsJunctions end junctions xs
                                                       | otherwise = bfsJunctions end junctions (next ++ xs)
    where next = map (\(p, d) -> (p, pos, Set.insert pos path, len + d)) $ filter (\(p,_) -> p /= prev && Set.notMember p path) $ junctions Map.! pos

part2 :: Solution
part2 input = let obstacles = Set.fromList [(r, c) | (r, row) <- zip [0..] input, (c, '#') <- zip [0..] row]
                  start = (0, 1)
                  end = (length input - 1, length (hd input) - 2)
                  (extra, jStart, jEnd, junctions) = findJunctions start end obstacles [(start, (-1, 1), start, 0)] Map.empty
              in V $ extra + bfsJunctions jEnd junctions [(jStart, (0,0), Set.empty, 0)]
