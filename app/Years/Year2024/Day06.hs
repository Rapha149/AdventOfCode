module Years.Year2024.Day06 (part1, part2) where

import Util.Util
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type State = (Vec, Vec)

parseInput :: [String] -> (Map Vec Bool, State)
parseInput input = (Map.fromList $ map (second (== '#')) points, (startPos, startDelta))
    where points = [((r, c), x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]
          (startPos, startDir) = hd $ filter ((`notElem` ".#") . snd) points
          startDelta = case startDir of
                            '^' -> (-1, 0)
                            '>' -> (0, 1)
                            'v' -> (1, 0)
                            '<' -> (0, -1)
                            _ -> error "Unknown direction."

getPath :: Map Vec Bool -> State -> Set Vec
getPath points (pos, delta@(dr, dc)) | next `Map.notMember` points = Set.singleton pos
                                     | points Map.! next = getPath points (pos, (dc, -dr))
                                     | otherwise = Set.insert pos $ getPath points (next, delta)
    where next = onBoth (+) pos delta

isLoop :: Map Vec Bool -> State -> Set State -> Bool
isLoop points state@(pos, delta@(dr, dc)) states | next `Map.notMember` points = False
                                              | state `Set.member` states = True
                                              | points Map.! next = isLoop points (pos, (dc, -dr)) states
                                              | otherwise = isLoop points (next, delta) $ Set.insert state states
    where next = onBoth (+) pos delta

part1 :: Solution
part1 = V . Set.size . uncurry getPath . parseInput

part2 :: Solution
part2 input = let (points, start) = parseInput input
              in V $ Set.size $ Set.filter (\p -> isLoop (Map.insert p True points) start Set.empty) $ getPath points start
