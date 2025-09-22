module Years.Year2024.Day16 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.Bifunctor
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ

type PrevMap = Map (Vec, Vec) (Int, [(Vec, Vec)])

findBestPaths :: Set Vec -> Vec -> PQueue Int (Vec, Vec, (Vec, Vec)) -> PrevMap -> PrevMap
findBestPaths obstacles end queue prevMap | PQ.null queue || maybe False ((<cost) . fst) (prevMap Map.!? (end, (0,0))) = prevMap
                                          | pos == end = findBestPaths obstacles end rest $ Map.insert (end, (0,0)) (cost, [prev]) prevMap
                                          | Set.member pos obstacles = findBestPaths obstacles end rest prevMap
                                          | Map.notMember (pos, delta) prevMap = findBestPaths obstacles end (foldr (uncurry PQ.insert) rest next) (Map.insert (pos, delta) (cost, [prev]) prevMap)
                                          | otherwise = findBestPaths obstacles end rest $ if otherCost < cost then prevMap else Map.insert (pos, delta) (cost, prev : otherPrev) prevMap
    where ((cost, (pos, delta@(dr, dc), prev)), rest) = fromJust $ PQ.minViewWithKey queue
          (otherCost, otherPrev) = prevMap Map.! (pos, delta)
          next = map (bimap (+cost) (\d -> (onBoth (+) pos d, d, (pos, delta)))) [(1, delta), (1001, (-dc, dr)), (1001, (dc, -dr))]

getPath :: PrevMap -> (Vec, Vec) -> Set Vec
getPath prevMap e@(pos,_) | Map.notMember e prevMap = Set.empty
                          | otherwise = Set.insert pos $ foldr (Set.union . getPath prevMap) Set.empty $ snd $ prevMap Map.! e

getResult :: (PrevMap -> (Vec, Vec) -> a) -> [String] -> a
getResult parse input = parse prevMap (end, (0,0))
    where maze = Map.fromList [((r, c), x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]
          start = fst $ Map.findMin $ Map.filter (== 'S') maze
          end = fst $ Map.findMin $ Map.filter (== 'E') maze
          obstacles = Map.keysSet $ Map.filter (== '#') maze
          prevMap = findBestPaths obstacles end (PQ.singleton 0 (start, (0, 1), ((-1, -1), (0, 0)))) Map.empty

part1 :: Solution
part1 = V . getResult (fst .: (Map.!))

part2 :: Solution
part2 = V . getResult (Set.size .: getPath)
