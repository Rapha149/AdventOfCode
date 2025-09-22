module Years.Year2021.Day15 (part1, part2) where

import Util.Util
import Data.Char
import Data.Maybe
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ

parseInput :: [String] -> Map Vec Int
parseInput input = Map.fromList $ [((r, c), digitToInt x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]

dijkstra :: Map Vec Int -> Vec -> PQueue Int Vec -> Set Vec -> Int
dijkstra levels end queue visited | PQ.null queue = error "No path found."
                                  | pos == end = cost
                                  | otherwise = dijkstra levels end (foldr (uncurry PQ.insert) rest next) (foldr (Set.insert . snd) visited next)
    where ((cost, pos), rest) = fromJust $ PQ.minViewWithKey queue
          next = [(cost + levels Map.! p, p) | d <- [(1, 0), (-1, 0), (0, 1), (0, -1)], let p = onBoth (+) pos d,
                                               Set.notMember p visited, Map.member p levels]

getLowestRisk :: Map Vec Int -> Int
getLowestRisk levels = dijkstra levels (fst $ Map.findMax levels) (PQ.singleton 0 (0, 0)) (Set.singleton (0, 0))

part1 :: Solution
part1 = V . getLowestRisk . parseInput


extend :: Map Vec Int -> Map Vec Int
extend levels = extend' (first (+ height)) 4 $ extend' (second (+ width)) 4 levels
    where (height, width) = both (+ 1) $ fst $ Map.findMax levels
          extend' :: (Vec -> Vec) -> Int -> Map Vec Int -> Map Vec Int
          extend' _ 0 m = m
          extend' f n m = Map.union m $ extend' f (n - 1) $ Map.map ((+ 1) . (`mod` 9)) $ Map.mapKeys f m

part2 :: Solution
part2 = V . getLowestRisk . extend . parseInput
