module Year2023.Day22 (part1, part2) where

import Util
import Data.List
import Data.List.Split
import Data.Tuple.Extra
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Vec3 = (Int, Int, Int)

parseInput :: [String] -> [[Vec3]]
parseInput [] = []
parseInput (l:ls) = [(x, y, z) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2], z <- [min z1 z2..max z1 z2]] : parseInput ls
    where ((x1, y1, z1), (x2, y2, z2)) = tuple $ map (triple . map read . splitOn ",") $ splitOn "~" l

fallDown :: Map Vec3 Int -> [Vec3] -> (Bool, [Vec3])
fallDown bricks cs | any ((== 0) . thd3) cs' = (True, cs)
                   | any (`Map.member` bricks) cs' = (False, cs)
                   | otherwise = fallDown bricks cs'
    where cs' = map (third3 (subtract 1)) cs

getSupports :: Map Vec3 Int -> [(Int, [Vec3])] -> Map Int [Int]
getSupports _ [] = Map.empty
getSupports bricks ((i, cubes):bs) = Map.insert i supports $ getSupports (foldr (`Map.insert` i) bricks fallen) bs
    where (onGround, fallen) = fallDown bricks cubes
          supports = if onGround then [0] else Set.toList $ Set.fromList $ Map.elems $ Map.restrictKeys bricks $ Set.fromList $ map (third3 (subtract 1)) fallen

countFalling :: Map Int [Int] -> [Int] -> Int
countFalling _ [] = 0
countFalling supports (i:is) = Map.size falling + countFalling supported (Map.keys falling ++ is)
    where (falling, supported) = Map.partition null $ Map.map (delete i) supports

part1 :: Solution
part1 input = let bricks = parseInput input
                  supports = Map.elems $ getSupports Map.empty $ zip [1..] $ sortOn (minimum . map thd3) bricks
              in V $ length $ filter (\i -> [i] `notElem` supports) [1..length bricks]

part2 :: Solution
part2 input = let bricks = parseInput input
                  supports = getSupports Map.empty $ zip [1..] $ sortOn (minimum . map thd3) bricks
              in V $ sum [countFalling supports [i] | i <- [1..length bricks]]
