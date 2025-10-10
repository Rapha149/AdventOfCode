module Years.Year2023.Day10 (part1, part2) where

import Util.Util
import Data.List
import Data.Bifunctor
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Diagram = Map Vec Char

parseInput :: [String] -> (Vec, Diagram)
parseInput input = (start, replaceStart diagram start)
    where diagram = Map.fromList [((r, c), x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]
          (start, _) = fromJust $ find (\(_, c) -> c == 'S') $ Map.toList diagram

replaceStart :: Diagram -> Vec -> Diagram
replaceStart diagram pos = Map.insert pos pipe diagram
    where pipe = fromJust $ find (all matches . getDirections) "|-LJ7F"
          matches :: Vec -> Bool
          matches delta | onBoth (+) pos delta `Map.notMember` diagram = False
                        | otherwise = diagram Map.! onBoth (+) pos delta `elem` (case delta of
                                       (-1, 0) -> "|7F"
                                       (0, 1) -> "-J7"
                                       (1, 0) -> "|LJ"
                                       (0, -1) -> "-LF"
                                       _ -> "Unknown direction.")

getDirections :: Char -> [Vec]
getDirections pipe = case pipe of
    '|' -> [(-1, 0), (1, 0)]
    '-' -> [(0, -1), (0, 1)]
    'L' -> [(-1, 0), (0, 1)]
    'J' -> [(-1, 0), (0, -1)]
    '7' -> [(1, 0), (0, -1)]
    'F' -> [(0, 1), (1, 0)]
    _ -> error "Unexpected character."

maxDistance :: Diagram -> [(Vec, Int)] -> Set Vec -> Int
maxDistance _ [] _ = 0
maxDistance diagram ((p, d):ps) visited = max d $ maxDistance diagram (ps ++ map (, d + 1) adjacents) (foldr Set.insert visited adjacents)
    where adjacents = filter (`Set.notMember` visited) $ map (onBoth (+) p) $ getDirections $ diagram Map.! p

findInside :: [Vec] -> Set Vec -> Set Vec
findInside [] unmarked = unmarked
findInside (p:xs) unmarked = findInside (adjacents ++ xs) (Set.difference unmarked $ Set.fromList adjacents)
    where adjacents = filter (`Set.member` unmarked) $ map (onBoth (+) p) [(-1, 0), (0, 1), (1, 0), (0, -1)]

part1 :: Solution
part1 input = V $ maxDistance diagram [(start, 0)] (Set.singleton start)
    where (start, diagram) = parseInput input


getPath :: Diagram -> [Vec] -> Set Vec -> [Vec]
getPath _ [] path = Set.toList path
getPath diagram (p:ps) path = getPath diagram (adjacents ++ ps) (foldr Set.insert path adjacents)
    where adjacents = filter (`Set.notMember` path) $ map (onBoth (+) p) $ getDirections $ diagram Map.! p

expand :: Diagram -> [Vec] -> [Vec]
expand _ [] = []
expand diagram ((r, c):xs) = (mr, mc) : map (bimap (+mr) (+mc)) (getDirections $ diagram Map.! (r, c)) ++ expand diagram xs
    where mr = r * 3 + 1
          mc = c * 3 + 1

part2 :: Solution
part2 input = V $ Set.size $ Set.filter (\(r, c) -> (r - 1) `mod` 3 == 0 && (c - 1) `mod` 3 == 0) inside
    where (start, diagram) = parseInput input
          path = expand diagram $ getPath diagram [start] (Set.singleton start)
          expandedCoords = Set.fromList [(r, c) | r <- [0..3 * length input - 1], c <- [0..3 * length (hd input) - 1]]
          inside = findInside [(0,0)] (Set.difference expandedCoords $ Set.fromList path)
