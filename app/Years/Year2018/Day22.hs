module Years.Year2018.Day22 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ

parseInput :: [String] -> (Int, Vec)
parseInput [depthInput, targetInput] = (depth, target)
    where depth = read (lst $ words depthInput) `mod` 20183
          target = pair $ map read $ splitOn "," $ lst $ words targetInput
parseInput _ = error "Invalid input"

getErosionLevel :: Int -> Vec -> Map Vec Int -> Vec -> Map Vec Int
getErosionLevel _ _ levels pos | pos `Map.member` levels = levels
getErosionLevel depth _ levels (0, 0) = Map.insert (0, 0) depth levels
getErosionLevel depth target levels pos | pos == target = Map.insert pos depth levels
getErosionLevel depth _ levels (x, 0) = Map.insert (x, 0) ((x * 16807 + depth) `mod` 20183) levels
getErosionLevel depth _ levels (0, y) = Map.insert (0, y) ((y * 48271 + depth) `mod` 20183) levels
getErosionLevel depth _ levels (x, y) = Map.insert (x, y) ((levels Map.! (x - 1, y) * levels Map.! (x, y - 1) + depth) `mod` 20183) levels

getErosionLevels :: Int -> Vec -> Vec -> Map Vec Int
getErosionLevels depth target (maxX, maxY) = foldl (getErosionLevel depth target) Map.empty [(x, y) | x <- [0..maxX], y <- [0..maxY]]

part1 :: Solution
part1 input = V $ sum $ Map.map (`mod` 3) $ getErosionLevels depth target target
    where (depth, target) = parseInput input

getFewestMinutes :: Int -> Vec -> Map Vec Int -> PQueue Int (Vec, Int) -> Set (Vec, Int) -> Int
getFewestMinutes depth target levels queue seen | null queue = error "No path found."
                                                | tool == regionType || state `Set.member` seen = getFewestMinutes depth target levels' rest seen
                                                | state == (target, 1) = cost
                                                | otherwise = getFewestMinutes depth target levels' (foldr (uncurry PQ.insert) rest next) (Set.insert state seen)
    where ((cost, state@(pos@(x, y), tool)), rest) = fromJust $ PQ.minViewWithKey queue
          levels' | pos `Map.member` levels = levels
                  | otherwise = foldl (getErosionLevel depth target) levels $ map (x, ) [0..y - 1] ++ map (, y) [0..x]
          regionType = levels' Map.! pos `mod` 3
          next = [(cost + 7, (pos, t)) | t <- [0..2], t /= tool, t /= regionType] ++
                 [(cost + 1, (p, tool)) | delta <- [(1, 0), (0, 1), (-1, 0), (0, -1)], let p = onBoth (+) pos delta, fst p >= 0 && snd p >= 0]

part2 :: Solution
part2 input = V $ getFewestMinutes depth target levels (PQ.singleton 0 ((0, 0), 1)) Set.empty
    where (depth, target@(tx, ty)) = parseInput input
          size = max tx ty + 100
          levels = getErosionLevels depth target (size, size)
