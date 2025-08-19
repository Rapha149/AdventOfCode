module Year2024.Day11 (part1, part2) where

import Util
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

changeNumber :: Int -> Int -> Map Int Int -> Map Int Int
changeNumber 0 v = Map.insertWith (+) 1 v
changeNumber k v | even $ length str = Map.insertWith (+) a v . Map.insertWith(+) b v
                 | otherwise = Map.insert (k * 2024) v
    where str = show k
          (a, b) = both read $ splitAt (length str `div` 2) str

countAfter :: Int -> [String] -> Int
countAfter blinks input = sum $ Map.elems $ foldr ($) numbers (replicate blinks (Map.foldrWithKey changeNumber Map.empty))
    where numbers = Map.fromListWith (+) $ map ((, 1) . read) $ words $ hd input

part1 :: Solution
part1 = V . countAfter 25

part2 :: Solution
part2 = V . countAfter 75
