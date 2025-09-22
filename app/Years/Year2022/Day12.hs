module Years.Year2022.Day12 (part1, part2) where

import Util.Util
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> (Vec, Vec, Map Vec Int)
parseInput input = (start, end, Map.fromList heights)
    where letters = [((r, c), x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]
          (start, end) = both (\x -> fst $ fromJust $ find ((== x) . snd) letters) ('S', 'E')
          heights = map (second (\case 'S' -> 0
                                       'E' -> 25
                                       x -> fromJust $ elemIndex x ['a'..'z'])) letters

revBfs :: Map Vec Int -> Set Vec -> [(Vec, Int)] -> Set Vec -> Int
revBfs _ _ [] _ = error "No path found."
revBfs heights ends ((pos, len):xs) visited | Set.member pos ends = len
                                            | otherwise = revBfs heights ends (xs ++ map (, len + 1) next) $ foldr Set.insert visited next
    where height = (heights Map.! pos) - 1
          next = filter (\p -> Set.notMember p visited && maybe False (>= height) (heights Map.!? p)) $
                 map (onBoth (+) pos) [(1, 0), (-1, 0), (0, 1), (0, -1)]

part1 :: Solution
part1 input = let (start, end, heights) = parseInput input
              in V $ revBfs heights (Set.singleton start) [(end, 0)] (Set.singleton end)

part2 :: Solution
part2 input = let (_, end, heights) = parseInput input
                  starts = Map.keysSet $ Map.filter (== 0) heights
              in V $ revBfs heights starts [(end, 0)] (Set.singleton end)
