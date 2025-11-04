module Years.Year2018.Day23 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ
import Text.Regex.TDFA

type Bot = (Vec3, Int)

parseInput :: [String] -> [Bot]
parseInput [] = []
parseInput (l:ls) = case getAllTextSubmatches $ l =~ "^pos=<(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)>, r=([0-9]+)$" of
                         [_, x, y, z, r] -> ((read x, read y, read z), read r) : parseInput ls
                         _ -> error "Invalid input."

getDistance :: Vec3 -> Vec3 -> Int
getDistance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

part1 :: Solution
part1 input = V $ length $ filter ((<= r) . getDistance pos . fst) bots
    where bots = parseInput input
          (pos, r) = maximumOn snd bots


type Range = (Int, Int)
data Cube = Cube { xRange :: Range, yRange :: Range, zRange :: Range } deriving (Eq, Ord, Show)
data CubeData = CubeData { cube :: Cube, fullyContainedBy :: Int, botsWithIntersection :: [Bot] }

distanceToFurthestPoint :: Vec3 -> Cube -> Int
distanceToFurthestPoint (x, y, z) Cube{..} = furthestDistance xRange x + furthestDistance yRange y + furthestDistance zRange z
    where furthestDistance :: Range -> Int -> Int
          furthestDistance (a, b) c = max (abs $ a - c) (abs $ b - c)

shortestDistanceToCube :: Vec3 -> Cube -> Int
shortestDistanceToCube (x, y, z) Cube{..} = shortestDistance xRange x + shortestDistance yRange y + shortestDistance zRange z
    where shortestDistance :: Range -> Int -> Int
          shortestDistance (a, b) c | c >= a && c <= b = 0
                                    | otherwise = min (abs $ a - c) (abs $ b - c)

splitCube :: Cube -> [Cube]
splitCube Cube{..} = Set.toList $ Set.fromList [
            Cube (clamp (x1, x1 + xHalf - 1)) (clamp (y1, y1 + yHalf - 1)) (clamp (z1, z1 + zHalf - 1)),
            Cube (clamp (x1, x1 + xHalf - 1)) (clamp (y1, y1 + yHalf - 1)) (z1 + zHalf, z2),
            Cube (clamp (x1, x1 + xHalf - 1)) (y1 + yHalf, y2) (clamp (z1, z1 + zHalf - 1)),
            Cube (clamp (x1, x1 + xHalf - 1)) (y1 + yHalf, y2) (z1 + zHalf, z2),
            Cube (x1 + xHalf, x2) (clamp (y1, y1 + yHalf - 1)) (clamp (z1, z1 + zHalf - 1)),
            Cube (x1 + xHalf, x2) (clamp (y1, y1 + yHalf - 1)) (z1 + zHalf, z2),
            Cube (x1 + xHalf, x2) (y1 + yHalf, y2) (clamp (z1, z1 + zHalf - 1)),
            Cube (x1 + xHalf, x2) (y1 + yHalf, y2) (z1 + zHalf, z2)
        ]
    where (x1, x2) = xRange
          (y1, y2) = yRange
          (z1, z2) = zRange
          xHalf = (x2 - x1 + 1) `div` 2
          yHalf = (y2 - y1 + 1) `div` 2
          zHalf = (z2 - z1 + 1) `div` 2
          clamp :: Range -> Range
          clamp (a, b) = (a, max a b)

getCube :: Int -> [Bot] -> Cube -> (Int, CubeData)
getCube parentFullyContainedBy bots cube = (fullyContainedBy + length botsWithIntersection, CubeData {..})
    where (fullyContainedBy, botsWithIntersection) = foldr checkBot (parentFullyContainedBy, []) bots
          checkBot :: Bot -> (Int, [Bot]) -> (Int, [Bot])
          checkBot (pos, r) (full, is) | r >= distanceToFurthestPoint pos cube = (full + 1, is) -- whole cube is within range of bot
                                       | r < shortestDistanceToCube pos cube = (full, is) -- whole cube is out of range of bot -> ignore
                                       | otherwise = (full, (pos, r):is)

getMaxBotsCubes :: PQueue Int CubeData -> Int -> [Cube] -> [Cube]
getMaxBotsCubes queue maxBots maxBotsCubes | null queue = maxBotsCubes
                                           | botCount < maxBots = getMaxBotsCubes rest maxBots maxBotsCubes
                                           | otherwise = getMaxBotsCubes (foldr (\(bc, cd) -> PQ.insert (-bc) cd) rest toSplit) maxBots' maxBotsCubes'
    where ((cost, cubeData), rest) = fromJust $ PQ.minViewWithKey queue
          botCount = -cost
          (finished, toSplit) = partition (null . botsWithIntersection . snd) $ filter ((>= maxBots) . fst) $ map (getCube cubeData.fullyContainedBy cubeData.botsWithIntersection) $ splitCube cubeData.cube
          (maxBots', maxBotsCubes') = case Map.lookupMax $ Map.fromListWith (++) $ map (second (singleton . cube)) finished of
                                           Nothing -> (maxBots, maxBotsCubes)
                                           Just (mx, cubes) -> (mx, if mx > maxBots then cubes else cubes ++ maxBotsCubes)

part2 :: Solution
part2 input = V $ minimum $ map (shortestDistanceToCube (0, 0, 0)) maxBotsCubes
    where bots = parseInput input
          (xs, ys, zs) = unzip3 $ map fst bots
          (botCount, cubeData) = getCube 0 bots $ Cube (minimum xs, maximum xs) (minimum ys, maximum ys) (minimum zs, maximum zs)
          maxBotsCubes = getMaxBotsCubes (PQ.singleton (-botCount) cubeData) 0 []
