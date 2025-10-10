module Years.Year2024.Day25 (part1) where

import Util.Util
import Data.Tuple.Extra
import Data.List.Extra

part1 :: Solution
part1 input = V $ sum [1 | lock <- locks, key <- keys, all ((< 6) . uncurry (+)) $ zip lock key]
    where (locks, keys) = both (map (map (subtract 1 . length . filter (== '#')) . transpose)) $
                          partition ((== "#####") . hd) $ split null input
