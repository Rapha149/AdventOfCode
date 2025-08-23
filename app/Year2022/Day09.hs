module Year2022.Day09 (part1, part2) where

import Util
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> [Vec]
parseInput = concatMap ((\(d, n) -> replicate (read n) $ dir d) . tuple . words)
    where dir :: String -> Vec
          dir "R" = (1, 0)
          dir "L" = (-1, 0)
          dir "U" = (0, 1)
          dir "D" = (0, -1)
          dir _ = error "Unknown direction."

move :: [Vec] -> [Vec] -> Set Vec
move _ [] = Set.empty
move rope (m:ms) = Set.insert (lst rope') $ move rope' ms
    where h = onBoth (+) m $ hd rope
          rope' = h : moveTail (tl rope) h
          moveTail :: [Vec] -> Vec -> [Vec]
          moveTail [] _ = []
          moveTail (r:rs) h' = let (dx, dy) = onBoth (-) h' r
                                   r' = onBoth (+) r (signum dx, signum dy)
                               in if abs dx <= 1 && abs dy <= 1 then r:rs else r' : moveTail rs r'

getVisited :: Int -> [String] -> Int
getVisited ropeLen = Set.size . Set.insert (0, 0) . move (replicate ropeLen (0, 0)) . parseInput

part1 :: Solution
part1 = V . getVisited 2

part2 :: Solution
part2 = V . getVisited 10
