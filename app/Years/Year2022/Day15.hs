module Years.Year2022.Day15 (part1, part2) where

import Util.Util
import Data.Bifunctor
import Data.Set (Set)
import qualified Data.Set as Set

distance :: Vec -> Vec -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parseInput :: [String] -> ([(Vec, Int)], Set Vec)
parseInput [] = ([], Set.empty)
parseInput (l:ls) = bimap ((sensor, distance sensor beacon):) (Set.insert beacon) $ parseInput ls
    where ws = words $ l ++ "," -- ini in getCoord
          getCoord :: Int -> Int
          getCoord = read . ini . drop 2 . (!!) ws
          sensor = (getCoord 2, getCoord 3)
          beacon = (getCoord 8, getCoord 9)

isCovered :: [(Vec, Int)] -> Vec -> Bool
isCovered sensors pos = any (\(s, d) -> distance s pos <= d) sensors

part1 :: Solution
part1 raw = V $ sum [1 | x <- [minX..maxX], Set.notMember (x, row) beacons, isCovered sensors (x, row)]
    where (row, input) = getExtraInt 2000000 raw
          (sensors, beacons) = parseInput input
          minX = minimum $ map (\((x,_), d) -> x - d) sensors
          maxX = maximum $ map (\((x,_), d) -> x + d) sensors

part2 :: Solution
part2 raw = V $ hd [x * 4000000 + y | ((x1, y1), d1) <- sensors, f1 <- [1, -1], let a = y1 - x1 + (d1 + 1) * f1,
                                      ((x2, y2), d2) <- sensors, f2 <- [1, -1], let b = x2 + y2 + (d2 + 1) * f2,
                                      even $ a + b, let x = (b - a) `div` 2, let y = (b + a) `div` 2,
                                      x >= 0 && y >= 0 && x <= area && y <= area, not $ isCovered sensors (x, y)]
    where (area, input) = getExtraInt 4000000 raw
          (sensors, _) = parseInput input
