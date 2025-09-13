module Year2021.Day22 (part1, part2) where

import Util
import Data.List.Extra hiding (intersect)
import Data.Tuple.Extra

type Range = (Int, Int)
type Region = (Range, Range, Range)

isEmpty :: Region -> Bool
isEmpty ((x1, x2), (y1, y2), (z1, z2)) = x1 > x2 || y1 > y2 || z1 > z2

size :: Region -> Int
size ((x1, x2), (y1, y2), (z1, z2)) = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

intersect :: Region -> Region -> Region
intersect ((x1, x2), (y1, y2), (z1, z2)) ((x1', x2'), (y1', y2'), (z1', z2')) = ((max x1 x1', min x2 x2'), (max y1 y1', min y2 y2'), (max z1 z1', min z2 z2'))

difference :: Region -> Region -> [Region]
difference r1@((x1, x2), (y1, y2), (z1, z2)) r2 | isEmpty i = [r1]
                                                | otherwise = carved
    where i@((ix1, ix2), (iy1, iy2), (iz1, iz2)) = r1 `intersect` r2
          carved = filter (not . isEmpty) [
                                              ((x1, ix1 - 1), (y1, y2), (z1, z2)), -- left
                                              ((ix2 + 1, x2), (y1, y2), (z1, z2)), -- right
                                              ((ix1, ix2), (y1, iy1 - 1), (z1, z2)), -- front
                                              ((ix1, ix2), (iy2 + 1, y2), (z1, z2)), -- back
                                              ((ix1, ix2), (iy1, iy2), (z1, iz1 - 1)), -- bottom
                                              ((ix1, ix2), (iy1, iy2), (iz2 + 1, z2)) -- top
                                          ]

parseInput :: [String] -> [(Bool, Region)]
parseInput [] = []
parseInput (l:ls) = (hd ws == "on", triple $ map (pair . map read . splitOn ".." . drop 2) $ splitOn "," $ lst ws) : parseInput ls
    where ws = words l

doSteps :: [Region] -> [(Bool, Region)] -> [Region]
doSteps regions [] = regions
doSteps regions ((False, region):xs) = doSteps (concatMap (`difference` region) regions) xs
doSteps regions ((True, region):xs) = doSteps (getOffPart regions region ++ regions) xs
    where getOffPart :: [Region] -> Region -> [Region]
          getOffPart [] r = [r]
          getOffPart (d:ds) r = concatMap (getOffPart ds) $ difference r d

part1 :: Solution
part1 = V . sumOn' size . doSteps [] . filter (not . isEmpty . snd) . map (second (intersect ((-50, 50), (-50, 50), (-50, 50)))) . parseInput

part2 :: Solution
part2 = V . sumOn' size . doSteps [] . parseInput
