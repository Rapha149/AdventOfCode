module Year2024.Day25 (part1) where

import Util
import Data.Tuple.Extra
import Data.List
import Data.List.Split

part1 :: Solution
part1 input = let (locks, keys) = both (map (map (subtract 1 . length . filter (== '#')) . transpose)) $
                                  partition ((== "#####") . hd) $ splitOn [""] input
              in V $ sum [1 | lock <- locks, key <- keys, all ((< 6) . uncurry (+)) $ zip lock key]
