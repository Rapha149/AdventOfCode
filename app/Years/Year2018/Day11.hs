module Years.Year2018.Day11 (part1, part2) where

import Util.Util
import Text.Printf
import Data.List.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

getPowerLevel :: Int -> Vec -> Int
getPowerLevel serial (x, y) = (rackId * y + serial) * rackId `div` 100 `mod` 10 - 5
    where rackId = x + 10

part1 :: Solution
part1 input = Msg $ fst $ maximumOn snd sums
    where serial = read $ hd input
          grid = Map.fromList [((x, y), getPowerLevel serial (x, y)) | x <- [1..300], y <- [1..300]]
          sums = [(printf "%d,%d" x y, sum [grid Map.! (x + dx, y + dy) | dx <- [0..2], dy <- [0..2]]) | x <- [1..298], y <- [1..298]]


(#!) :: Ord k => Map k Int -> k -> Int
(#!) = flip (Map.findWithDefault 0)

getSummedAreaTable :: Int -> [Vec] -> Map Vec Int -> Map Vec Int
getSummedAreaTable _ [] sat = sat
getSummedAreaTable serial ((x, y):xs) sat = getSummedAreaTable serial xs $ Map.insert (x, y) level sat
    where level = getPowerLevel serial (x, y) + sat #! (x - 1, y) + sat #! (x, y - 1) - sat #! (x - 1, y - 1)

part2 :: Solution
part2 input = Msg $ fst $ maximumOn snd sums
    where sat = getSummedAreaTable (read $ hd input) [(x, y) | x <- [1..300], y <- [1..300]] Map.empty
          sums = [(printf "%d,%d,%d" (x - size + 1) (y - size + 1) size,
                   sat #! (x, y) - sat #! (x - size, y) - sat #! (x, y - size) + sat #! (x - size, y - size)) |
                  size <- [1..300], x <- [size..300], y <- [size..300]]
