module Year2024.Day18 (part1, part2) where

import Util
import Data.Maybe
import Data.List.Split
import Text.Printf
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> ([Vec], Set Vec, Int, Int)
parseInput input = (bytes, points, size, count)
    where custom = ',' `notElem` hd input
          (size, count) = if custom then tuple $ map read $ words $ hd input else (70, 1024)
          bytes = map (tuple . map read . splitOn ",") $ (if custom then tl else id) input
          points = Set.fromList [(x, y) | x <- [0..size], y <- [0..size]]

bfs :: Int -> Set Vec -> [[Vec]] -> Maybe [Vec]
bfs _ _ [] = Nothing
bfs size points (path:ps) | hd path == (size, size) = Just path
                          | otherwise = bfs size (foldr Set.delete points adjacents) (ps ++ map (:path) adjacents)
    where adjacents = filter (`Set.member` points) $ map (onBoth (+) $ hd path) [(-1, 0), (1, 0), (0, -1), (0, 1)]

firstBlocking :: Int -> Set Vec -> Set Vec -> [Vec] -> Vec
firstBlocking _ _ _ [] = error "No blocking obstacle."
firstBlocking size points lastPath (b:bs) | Set.notMember b lastPath = firstBlocking size newPoints lastPath bs
                                          | otherwise = case bfs size newPoints [[(0,0)]] of
                                                             (Just path) -> firstBlocking size newPoints (Set.fromList path) bs
                                                             Nothing -> b
    where newPoints = Set.delete b points

part1 :: Solution
part1 input = let (bytes, points, size, count) = parseInput input
              in V $ subtract 1 $ length $ fromJust $ bfs size (foldr Set.delete points $ take count bytes) [[(0,0)]]

part2 :: Solution
part2 input = let (bytes, points, size, _) = parseInput input
              in Msg $ uncurry (printf "%d,%d") $ firstBlocking size points points bytes
