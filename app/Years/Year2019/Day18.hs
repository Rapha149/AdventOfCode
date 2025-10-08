module Years.Year2019.Day18 (part1, part2) where

import Util.Util
import Data.Char
import Data.Bits
import Data.Word
import Data.Maybe
import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ

type Graph = Map Int [(Int, Int, Word32)]

parseInput :: [String] -> Map Vec Char
parseInput input = Map.fromList [((x, y), c) | (y, row) <- zip [0..] input, (x, c) <- zip [0..] row]

buildGraph :: Map Vec Char -> Graph
buildGraph grid = Map.map (\pos -> reachable [(pos, 0, 0)] (Set.singleton pos)) $ Map.fromList pois
    where starts = Map.keys $ Map.filter (== '@') grid
          pois = zip [27..] starts ++ map (\(pos, c) -> (ord c - ord 'a', pos)) (Map.toList $ Map.filter (\c -> c `notElem` ".#@" && isLower c) grid)
          reachable :: [(Vec, Int, Word32)] -> Set Vec -> [(Int, Int, Word32)]
          reachable [] _ = []
          reachable ((pos, cost, doors):xs) seen | c == '#' = reachable xs seen
                                                 | cost == 0 || c `elem` ".@" = reachable (xs ++ map (, cost + 1, doors) next) seen'
                                                 | isLower c = (ord c - ord 'a', cost, doors) : reachable (xs ++ map (, cost + 1, doors) next) seen'
                                                 | otherwise = let doors' = setBit doors $ ord c - ord 'A'
                                                               in reachable (xs ++ map (, cost + 1, doors') next) seen'
              where c = grid Map.! pos
                    next = filter (`Set.notMember` seen) $ map (onBoth (+) pos) [(1, 0), (-1, 0), (0, 1), (0, -1)]
                    seen' = foldr Set.insert seen next

bfs1 :: Graph -> PQueue Int (Int, Word32) -> Set (Int, Word32) -> Int
bfs1 graph queue seen | null queue = error "No solution found."
                      | missingKeys == 0 = cost
                      | Set.member (node, missingKeys) seen = bfs1 graph rest seen
                      | otherwise = bfs1 graph (PQ.union rest $ PQ.fromList next) (Set.insert (node, missingKeys) seen)
    where ((cost, (node, missingKeys)), rest) = fromJust $ PQ.minViewWithKey queue
          next = [(cost + cost', (node', clearBit missingKeys node')) | (node', cost', doors) <- graph Map.! node, doors .&. missingKeys == 0, testBit missingKeys node']

part1 :: Solution
part1 input = let graph = buildGraph $ parseInput input
                  keys = foldl setBit 0 $ Map.keys $ Map.delete 27 graph
              in V $ bfs1 graph (PQ.singleton 0 (27, keys)) Set.empty


bfs2 :: Graph -> PQueue Int (Seq Int, Word32) -> Set (Seq Int, Word32) -> Int
bfs2 graph queue seen | null queue = error "No solution found."
                      | missingKeys == 0 = cost
                      | Set.member (robots, missingKeys) seen = bfs2 graph rest seen
                      | otherwise = bfs2 graph (PQ.union rest $ PQ.fromList next) (Set.insert (robots, missingKeys) seen)
    where ((cost, (robots, missingKeys)), rest) = fromJust $ PQ.minViewWithKey queue
          next = concatMap getNext [0..length robots - 1]
          getNext :: Int -> [(Int, (Seq Int, Word32))]
          getNext i = let node = robots `Seq.index` i
                      in [(cost + cost', (Seq.update i node' robots, clearBit missingKeys node')) |
                            (node', cost', doors) <- graph Map.! node, doors .&. missingKeys == 0, testBit missingKeys node']

part2 :: Solution
part2 input = let grid = parseInput input
                  (x, y) = fst $ Map.findMin $ Map.filter (== '@') grid
                  graph = buildGraph $ foldr (uncurry Map.insert) grid [((x, y), '#'),
                                                                        ((x - 1, y), '#'), ((x + 1, y), '#'), ((x, y - 1), '#'), ((x, y + 1), '#'),
                                                                        ((x - 1, y - 1), '@'), ((x + 1, y - 1), '@'), ((x - 1, y + 1), '@'), ((x + 1, y + 1), '@')]
                  (keys, robots) = partition (<= 26) $ Map.keys graph
              in V $ bfs2 graph (PQ.singleton 0 (Seq.fromList robots, foldl setBit 0 keys)) Set.empty
