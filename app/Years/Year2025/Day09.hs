module Years.Year2025.Day09 (part1, part2) where

import Util.Util
import Data.List
import Data.List.Split

parseInput :: [String] -> [Vec]
parseInput = map (pair . map read . splitOn ",")

getMaxRectangle :: (Vec -> Vec -> Bool) -> [Vec] -> Int
getMaxRectangle test corners = getMax [(a, b) | (a:bs) <- tails corners, b <- bs] 0
    where getSize :: Vec -> Vec -> Int
          getSize (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
          getMax :: [(Vec, Vec)] -> Int -> Int
          getMax [] mx = mx
          getMax ((a, b):xs) mx = let size = getSize a b
                                  in getMax xs $ if size > mx && test a b then size else mx

part1 :: Solution
part1 = V . getMaxRectangle (\_ _ -> True) . parseInput


noIntersection :: [(Vec, Vec)] -> Vec -> Vec -> Bool
noIntersection edges (x1, y1) (x2, y2) = all inside edges
    where inside :: (Vec, Vec) -> Bool
          inside ((ex1, ey1), (ex2, ey2)) = (max ex1 ex2 <= min x1 x2) ||
                                            (min ex1 ex2 >= max x1 x2) ||
                                            (max ey1 ey2 <= min y1 y2) ||
                                            (min ey1 ey2 >= max y1 y2)

part2 :: Solution
part2 input = V $ getMaxRectangle (noIntersection edges) corners
    where corners = parseInput input
          edges = zip corners (tl corners ++ [hd corners])
