module Years.Year2022.Day24 (part1, part2) where

import Util.Util
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> (Vec, Vec, Int, Int, [(Vec, Vec)])
parseInput input = (start, end, length input - 2, length (hd input) - 2, blizzards)
    where start = (-1, fromJust (elemIndex '.' $ hd input) - 1)
          end = (length input - 2, fromJust (elemIndex '.' $ lst input) - 1)
          blizzards = [((r, c), dir) | (r, row) <- zip [-1..] input, (c, x) <- zip [-1..] row, x `elem` "^>v<",
                                       let dir = case x of '^' -> (-1, 0); '>' -> (0, 1); 'v' -> (1, 0); '<' -> (0, -1); _ -> error "Invalid direction." ]

getShortestTime :: Int -> Int -> Vec -> Vec -> [(Vec, Vec)] -> Int -> Set Vec -> Int
getShortestTime height width start end blizzards time positions | Set.member end next = time
                                                                | otherwise = getShortestTime height width start end blizzards (time + 1) next
    where bsNow = Set.fromList $ map (\((r, c), (dr, dc)) -> ((r + time * dr) `mod` height, (c + time * dc) `mod` width)) blizzards
          getNext :: Vec -> [Vec]
          getNext pos = filter (\p -> Set.notMember p bsNow && (p == start || p == end || inBounds (0, 0) (height, width) p)) $ map (onBoth (+) pos) [(1, 0), (0, 1), (-1, 0), (0, -1), (0, 0)]
          next = Set.foldr (Set.union . Set.fromList . getNext) Set.empty positions

part1 :: Solution
part1 input = let (start, end, height, width, blizzards) = parseInput input
              in V $ getShortestTime height width start end blizzards 1 (Set.singleton start)

part2 :: Solution
part2 input = let (start, end, height, width, blizzards) = parseInput input
                  getTime :: Int -> (Vec, Vec) -> Int
                  getTime time (s, e) = getShortestTime height width s e blizzards (time + 1) (Set.singleton s)
              in V $ foldl getTime 0 [(start, end), (end, start), (start, end)]
