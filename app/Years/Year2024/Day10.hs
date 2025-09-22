module Years.Year2024.Day10 (part1, part2) where

import Util.Util
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Points = Map Vec (Int, [Vec])

count :: (Vec -> Points -> Points) -> Points -> [Vec] -> Int
count _ _ [] = 0
count delete points (p:ps) | Map.notMember p points = count delete points ps
                           | height == 9 = 1 + count delete (delete p points) ps
                           | otherwise = count delete (delete p points) (adjacents ++ ps)
    where (height, adjacents) = points Map.! p

sumCounts :: (Vec -> Points -> Points) -> [String] -> Int
sumCounts delete input = sum $ map (count delete points . (:[])) $ Map.keys $ Map.filter ((== 0) . fst) points
    where heights = Map.fromList [((r, c), digitToInt x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]
          points = Map.mapWithKey (\p h -> (h, filter ((== Just (h+1)) . (Map.!?) heights) $ map (onBoth (+) p) [(-1, 0), (1, 0), (0, -1), (0, 1)])) heights

part1 :: Solution
part1 = V . sumCounts Map.delete

part2 :: Solution
part2 = V . sumCounts seq
