module Years.Year2024.Day20 (part1, part2) where

import Util.Util
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

getPath :: Set Vec -> Vec -> Vec -> Vec -> [Vec]
getPath obstacles prev cur end | cur == end = [cur]
                               | otherwise = cur : getPath obstacles cur next end
    where next = fromJust $ find (\p -> prev /= p && Set.notMember p obstacles) $ map (onBoth (+) cur) [(-1, 0), (1, 0), (0, -1), (0, 1)]

getCheats :: Int -> Int -> [(Int, Vec)] -> Int
getCheats _ _ [] = 0
getCheats maxDistance minDiff ((i, p):ps) = sumOn' (fromEnum . isCheat) ps + getCheats maxDistance minDiff ps
    where isCheat :: (Int, Vec) -> Bool
          isCheat (j, o) = let distance = uncurry (+) $ onBoth (abs .: (-)) p o
                               diff = j - i - distance
                           in distance <= maxDistance && diff >= minDiff

getResult :: Int -> [String] -> Int
getResult maxDistance rawInput = getCheats maxDistance minDiff $ zip [0..] $ getPath obstacles (-1, -1) start end
    where (minDiff, input) = getExtraInt 100 rawInput
          grid = [((r, c), x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]
          obstacles = Set.fromList $ map fst $ filter ((== '#') . snd) grid
          (start, end) = pair $ map (\c -> fst $ fromJust $ find ((== c) . snd) grid) "SE"

part1 :: Solution
part1 = V . getResult 2

part2 :: Solution
part2 = V . getResult 20
