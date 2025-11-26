module Years.Year2017.Day03 (part1, part2) where

import Util.Util
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

part1 :: Solution
part1 input | n == 1 = V 0
            | otherwise = V $ track + abs (n - middle)
    where n = read $ hd input
          track = ceiling (sqrt $ fromIntegral n :: Double) `div` 2
          start = (track * 2 - 1) ^ (2 :: Int) + 1
          middle = (n - start) `div` (track * 2) * track * 2 + start + track - 1


moves :: [Vec]
moves = concat [(0, 1) : replicate (n - 1) (1, 0) ++ replicate n (0, -1) ++ replicate n (-1, 0) ++ replicate n (0, 1) | n <- [2, 4..]]

getFirstLarger :: Int -> Vec -> Map Vec Int -> [Vec] -> Int
getFirstLarger _ _ _ [] = error "Empty moves list."
getFirstLarger minValue (x, y) numbers ((mx, my):ms) | n >= minValue = n
                                                     | otherwise = getFirstLarger minValue (x + mx, y + my) (Map.insert (x, y) n numbers) ms
    where n = max 1 $ sum [a | dx <- [-1..1], dy <- [-1..1], Just a <- [numbers Map.!? (x + dx, y + dy)]]

part2 :: Solution
part2 input = V $ getFirstLarger (read $ hd input) (0, 0) Map.empty moves
