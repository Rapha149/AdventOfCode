module Years.Year2021.Day17 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra

getStartVelocities :: [String] -> [Vec]
getStartVelocities input = [(x, y) | x <- [min 0 fartherX .. max 0 fartherX], y <- [fartherY .. abs fartherY * 2], hits (0, 0) (x, y)]
    where (xs, ys) = pair $ map (map read . splitOn ".." . drop 2) $ splitOn ", " $ drop 13 $ hd input
          ((minX, maxX), (minY, maxY)) = both pair (xs, ys)
          (fartherX, fartherY) = both (maximumOn abs) (xs, ys)
          hits :: Vec -> Vec -> Bool
          hits (x, y) (vx, vy) | vy < 0 && y < minY = False
                               | x >= minX && x <= maxX && y >= minY && y <= maxY = True
                               | otherwise = hits (x + vx, y + vy) (vx - signum vx, vy - 1)

part1 :: Solution
part1 = V . (\n -> n * (n + 1) `div` 2) . maximum . map snd . getStartVelocities

part2 :: Solution
part2 = V . length . getStartVelocities
