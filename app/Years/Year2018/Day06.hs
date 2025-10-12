module Years.Year2018.Day06 (part1, part2) where

import Util.Util
import Data.List.Extra
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

parseInput :: [String] -> [Vec]
parseInput = map (pair . map read . splitOn ", ")

manhattan :: Vec -> Vec -> Int
manhattan (px, py) (qx, qy) = abs (px - qx) + abs (py - qy)

getClosestPoint :: [Vec] -> Vec -> Maybe Vec
getClosestPoint points p = case [q | (q, d) <- distances, d == minDistance] of
                                [q] -> Just q
                                _ -> Nothing
    where distances = map (toSnd (manhattan p)) points
          minDistance = minimum $ map snd distances

part1 :: Solution
part1 input = V $ maximum $ Map.fromListWith (+) [(closest, 1) | x <- [minX..maxX], y <- [minY..maxY],
                                                                 Just closest <- [getClosestPoint points (x, y)],
                                                                 Set.notMember closest infinitePoints]
    where points = parseInput input
          ((minX, maxX), (minY, maxY)) = getBounds points
          infinitePoints = Set.fromList [p | x <- [minX..maxX], y <- [minY..maxY],
                                             x == minX || x == maxX || y == minY || y == maxY,
                                             Just p <- [getClosestPoint points (x, y)]]

part2 :: Solution
part2 raw = V $ length [(x, y) | x <- [minX..maxX], y <- [minY..maxY], sumOn' (manhattan (x, y)) points < maxTotal]
    where (maxTotal, input) = getExtraInt 10000 raw
          points = parseInput input
          ((minX, maxX), (minY, maxY)) = getBounds points
