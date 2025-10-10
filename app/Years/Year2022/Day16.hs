module Years.Year2022.Day16 (part1, part2) where

import Util.Util
import Data.Bits
import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

type Node = String
type Edge = (Node, Node)
type Time = Int
type Mask = Int

edge :: Node -> Node -> Edge
edge a b = (min a b, max a b)

parseInput :: [String] -> (Map Node Int, Map Edge Int)
parseInput [] = (Map.empty, Map.empty)
parseInput (l:ls) = bimap (Map.insert node rate) (Map.union edges) $ parseInput ls
    where ws = words l
          node = ws !! 1
          rate = read $ ini $ drop 5 $ ws !! 4
          edges = Map.fromList $ map ((, 1) . edge node) $ splitOn "," $ concat $ drop 9 ws

floyd :: [Node] -> Map Edge Int -> Map Edge Int
floyd nodes edges = foldl updateMin edges [(k, i, j) | k <- nodes, (i:is) <- tails nodes, j <- is]
    where updateMin :: Map Edge Int -> (Node, Node, Node) -> Map Edge Int
          updateMin es (k, i, j) | Map.notMember (edge k i) es || Map.notMember (edge k j) es = es
                                 | otherwise = Map.insertWith min (i, j) (es Map.! edge k i + es Map.! edge k j) es

dfs :: Map Node Int -> Map Edge Int -> [(Int, Node)] -> Node -> Time -> Mask -> Int
dfs flow paths indices node time opened = foldr (max . open) 0 moves
    where moves = filter (not . testBit opened . fst) indices
          open :: (Int, Node) -> Int
          open (i, n) = let cost = paths Map.! edge node n + 1
                            newTime = time - cost
                            gained = (flow Map.! n) * newTime
                        in if cost >= time then 0 else gained + dfs flow paths indices n newTime (setBit opened i)

part1 :: Solution
part1 input = V $ dfs flow paths indices "AA" 30 0
    where (nodes, edges) = parseInput input
          paths = floyd (Map.keys nodes) edges
          flow = Map.filter (> 0) nodes
          indices = zip [0..] $ Map.keys flow


dfsAll :: Map Node Int -> Map Edge Int -> [(Int, Node)] -> Node -> Time -> Mask -> Int -> IntMap Int
dfsAll flow paths indices node time opened pressure = foldr (IM.unionWith max . open) (IM.singleton opened pressure) moves
    where moves = filter (not . testBit opened . fst) indices
          open :: (Int, Node) -> IntMap Int
          open (i, n) = let cost = paths Map.! edge node n + 1
                            newTime = time - cost
                            gained = (flow Map.! n) * newTime
                        in if cost >= time then IM.empty else dfsAll flow paths indices n newTime (setBit opened i) (pressure + gained)

part2 :: Solution
part2 input = V $ maximum [p1 + p2 | ((m1, p1):xs) <- tails pressures, (m2, p2) <- xs, m1 .&. m2 == 0]
    where (nodes, edges) = parseInput input
          paths = floyd (Map.keys nodes) edges
          flow = Map.filter (> 0) nodes
          indices = zip [0..] $ Map.keys flow
          pressures = IM.toList $ dfsAll flow paths indices "AA" 26 0 0
