module Year2023.Day25 (part1) where

import Util
import Control.Arrow
import Data.List
import Data.List.Split
import Data.List.Extra (maximumOn)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Node = String
type Edges = Map Node (Map Node Int)

maximumAdjacency :: Edges -> Map Node Int -> [Node] -> Int -> (Node, Node, Int)
maximumAdjacency edges _ [] _ = maximumAdjacency rest nodes [node] 0
    where ((node, nodes), rest) = fromJust $ Map.minViewWithKey edges
maximumAdjacency edges nodes found weight | Map.null nodes = (found !! 1, hd found, weight)
                                          | otherwise = maximumAdjacency (Map.delete next edges) nodes' (next:found) weight'
    where (next, weight') = maximumOn snd $ Map.toList nodes
          nodes' = Map.delete next $ Map.foldrWithKey (Map.insertWith (+)) nodes $ Map.restrictKeys (edges Map.! next) $ Map.keysSet edges

merge :: Edges -> Node -> Node -> Edges
merge edges a b = Map.insert a (Map.unionWith (+) aEdges bEdges) $ Map.foldrWithKey (\n w -> Map.adjust (Map.insertWith (+) a w . Map.delete b) n) edges bEdges
    where aEdges = Map.delete b $ edges Map.! a
          bEdges = Map.delete a $ edges Map.! b

minCut :: Edges -> [Node] -> Map Node (Set Node) -> Set Node -> Int -> Set Node
minCut _ [] _ _ _ = error "No nodes."
minCut _ [_] _ best _ = best
minCut edges nodes members best bestWeight = minCut edges' nodes' members' best' bestWeight'
    where (s, t, weight) = maximumAdjacency edges Map.empty [] 0
          nodes' = delete t nodes
          edges' = merge edges s t
          members' = Map.delete t $ Map.insertWith Set.union s (members Map.! t) members
          (best', bestWeight') = if weight < bestWeight then (members Map.! t, weight) else (best, bestWeight)

lineToEdges :: String -> Edges
lineToEdges line = Map.fromList $ (a, Map.fromList $ map (, 1) bs) : map (, Map.singleton a 1) bs
    where (a, bs) = second (splitOn " ") $ pair $ splitOn ": " line

part1 :: Solution
part1 input = let edges = foldr (Map.unionWith Map.union . lineToEdges) Map.empty input
                  nodes = Map.keys edges
                  size = Set.size $ minCut edges nodes (Map.fromList $ map (id &&& Set.singleton) nodes) Set.empty maxBound
              in V $ size * (length nodes - size)
