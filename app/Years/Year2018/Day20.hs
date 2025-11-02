module Years.Year2018.Day20 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ

data RegexPart  = Direction Vec | Branches [Regex] deriving Show
type Regex = [RegexPart]

parseInput :: [String] -> Regex
parseInput ['^':input] = case parseRegex input [] of
                              (regex, "$") -> regex
                              _ -> error "Not whole string was parsed."
    where parseRegex :: String -> Regex -> (Regex, String)
          parseRegex "" acc = (reverse acc, "")
          parseRegex (c:cs) acc | c `elem` ")|$" = (reverse acc, c:cs)
                                | otherwise = let (part, rest) = case c of
                                                                       '(' -> parseGroup cs []
                                                                       'N' -> (Direction (0, 1), cs)
                                                                       'E' -> (Direction (1, 0), cs)
                                                                       'S' -> (Direction (0, -1), cs)
                                                                       'W' -> (Direction (-1, 0), cs)
                                                                       _ -> error "Invalid input."
                                              in parseRegex rest (part:acc)
          parseGroup :: String -> [Regex] -> (RegexPart, String)
          parseGroup cs acc = let (regex, rest) = parseRegex cs []
                                  acc' = regex : acc
                              in case rest of
                                      (')':rs) -> (Branches $ reverse acc', rs)
                                      ('|':rs) -> parseGroup rs acc'
                                      _ -> error "Unclosed group."
parseInput _ = error "Invalid input."

getShortestPaths :: PQueue Int (Vec, Regex) -> Map Vec Int -> Map Vec Int
getShortestPaths queue costs | null queue = costs
                             | maybe False (< cost) $ costs Map.!? pos = getShortestPaths rest costs
                             | otherwise = case regex of
                                                [] -> getShortestPaths rest costs'
                                                (Direction delta:xs) -> getShortestPaths (PQ.insert (cost + 1) (onBoth (+) pos delta, xs) rest) costs'
                                                (Branches rs:xs) -> getShortestPaths (foldr (PQ.insert cost) rest [(pos, r ++ xs) | r <- rs]) costs'
    where ((cost, (pos, regex)), rest) = fromJust $ PQ.minViewWithKey queue
          costs' = Map.insert pos cost costs

part1 :: Solution
part1 input = V $ maximum $ getShortestPaths (PQ.singleton 0 ((0, 0), regex)) Map.empty
    where regex = parseInput input

part2 :: Solution
part2 input = V $ length $ Map.filter (>= 1000) $ getShortestPaths (PQ.singleton 0 ((0, 0), regex)) Map.empty
    where regex = parseInput input
