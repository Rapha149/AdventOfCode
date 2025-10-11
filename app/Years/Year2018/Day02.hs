module Years.Year2018.Day02 (part1, part2) where

import Util.Util
import Data.List
import qualified Data.Map.Strict as Map

part1 :: Solution
part1 input = V $ length (filter (elem 2) counts) * length (filter (elem 3) counts)
    where counts = map (Map.elems . Map.fromListWith (+) . map (, 1 :: Int)) input

part2 :: Solution
part2 input = Msg $ hd [map fst equal | (x:ys) <- tails input, y <- ys, let (equal, diff) = partition (uncurry (==)) $ zip x y, length diff <= 1]
