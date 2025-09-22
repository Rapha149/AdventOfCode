module Year2019.Day10 (part1, part2) where

import Util
import Data.List.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

normalize :: Vec -> Vec
normalize (x, y) = (x `div` d, y `div` d)
    where d = gcd x y

getMaxLinesOfSight :: [String] -> Map Vec [Vec]
getMaxLinesOfSight input = maximumOn Map.size $ map getLinesOfSight asteroids
    where asteroids = [(c, r) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row, x == '#']
          getLinesOfSight :: Vec -> Map Vec [Vec]
          getLinesOfSight asteroid = Map.map (map fst . sortOn snd) $ Map.fromListWith (++) [(normalize v, [(ast, abs vx + abs vy)]) | ast <- asteroids, ast /= asteroid, let v@(vx, vy) = onBoth (-) ast asteroid]

part1 :: Solution
part1 = V . Map.size . getMaxLinesOfSight


getAngle :: Vec -> Double
getAngle (x, y) | angle < 0 = angle + 2 * pi
                | otherwise = angle
    where angle = atan2 (fromIntegral x) (- fromIntegral y)

vaporize :: Map Vec [Vec] -> [Vec] -> [Vec]
vaporize _ [] = error "Empty direction list."
vaporize linesOfSights (d:ds) = case linesOfSights Map.! d of
                                     [] -> vaporize linesOfSights ds
                                     (x:xs) -> x : vaporize (Map.insert d xs linesOfSights) ds

part2 :: Solution
part2 input = let linesOfSights = getMaxLinesOfSight input
                  order = sortOn getAngle $ Map.keys linesOfSights
                  (x, y) = vaporize linesOfSights (cycle order) !! 199
              in V $ x * 100 + y
