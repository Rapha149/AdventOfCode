module Year2021.Day12 (part1, part2) where

import Util
import Data.Char
import Data.List.Extra
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Node = String
type Nodes = Map Node (Bool, [Node])

parseInput :: [String] -> Nodes
parseInput [] = Map.empty
parseInput (l:ls) = ins n1 n2 g1 $ ins n2 n1 g2 $ parseInput ls
    where (n1, n2) = pair $ splitOn "-" l
          (g1, g2) = both (isUpper . hd) (n1, n2)
          ins :: Node -> Node -> Bool -> Nodes -> Nodes
          ins node adj large = Map.alter (Just . maybe (large, [adj]) (second (adj:))) node

getPaths :: String -> [([Node], Nodes)] -> Set [Node]
getPaths _ [] = Set.empty
getPaths twice ((path, nodes):xs) | node == "end" = Set.insert path $ getPaths twice xs
                                  | otherwise = getPaths twice (next ++ xs)
    where node = hd path
          (large, adjacent) = nodes Map.! node
          nodes' | large = nodes
                 | node == twice && node `notElem` tl path = nodes
                 | otherwise = Map.delete node nodes
          next = map ((, nodes') . (:path)) $ filter (`Map.member` nodes) adjacent

part1 :: Solution
part1 = V . Set.size . getPaths "" . singleton . (["start"],) . parseInput

part2 :: Solution
part2 input = let nodes = parseInput input
                  small = Map.keys (Map.filter (not . fst) nodes) \\ ["start", "end"]
              in V $ Set.size $ foldr (Set.union . (`getPaths` [(["start"], nodes)])) Set.empty small
