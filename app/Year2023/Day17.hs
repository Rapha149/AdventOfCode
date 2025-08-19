module Year2023.Day17 (part1, part2) where

import Util
import Data.Char
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ

type State = (Vec, Vec, Int)

getMinHeatLoss :: (Vec -> Int -> [(Vec, Int)]) -> (Int -> Bool) -> [String] -> Int
getMinHeatLoss getMoves canFinish input = findMinPath getMoves (\p straight -> p == target && canFinish straight) weights (PQ.singleton 0 ((0,0), (0,1), 0)) (Set.singleton ((0,0), (0,1), 0))
    where weights = Map.fromList [((r, c), digitToInt x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]
          target = fst $ Map.findMax weights

findMinPath :: (Vec -> Int -> [(Vec, Int)]) -> (Vec -> Int -> Bool) -> Map Vec Int -> PQueue Int State -> Set State -> Int
findMinPath getMoves isFinish weights queue visited | PQ.null queue = error "No path found."
                                                    | isFinish pos straight = cost
                                                    | otherwise = findMinPath getMoves isFinish weights (foldr (\x@(p,_,_) -> PQ.insert (cost + weights Map.! p) x) rest adjacents) (foldr Set.insert visited adjacents)
    where ((cost, (pos, move, straight)), rest) = fromJust $ PQ.minViewWithKey queue
          adjacents = filter (\state@(p,_,_) -> Map.member p weights && Set.notMember state visited) $ map (\(d, s) -> (onBoth (+) pos d, d, s)) $ getMoves move straight

part1 :: Solution
part1 = V . getMinHeatLoss (\(dr, dc) straight -> ((dc, dr), 1) : ((-dc, -dr), 1) : [((dr, dc), straight + 1) | straight < 3]) (const True)

part2 :: Solution
part2 = V . getMinHeatLoss (\(dr, dc) straight -> [((dr, dc), straight + 1) | straight < 10] ++ if straight >= 4 then [((dc, dr), 1), ((-dc, -dr), 1)] else []) (>= 4)
