module Years.Year2018.Day25 (part1) where

import Util.Util
import Data.List.Extra

type Point = [Int]

getConstellations :: [Point] -> [Point] -> Int
getConstellations [] [] = 0
getConstellations [] (p:ps) = 1 + getConstellations [p] ps
getConstellations (x:xs) ps = getConstellations (xs ++ xs') ps'
    where (xs', ps') = partition ((<= 3) . distance) ps
          distance :: Point -> Int
          distance = sumOn' abs . zipWith (-) x

part1 :: Solution
part1 = V . getConstellations [] . map (map read . splitOn ",")
