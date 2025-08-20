module Year2022.Day03 (part1, part2) where

import Util
import Data.List
import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe
import qualified Data.Set as Set

getPriority :: [Char] -> Int
getPriority [c] = 1 + fromJust (elemIndex c $ ['a'..'z'] ++ ['A'..'Z'])
getPriority _ = error "Intersect is empty or has more than one element."

getErrorPriority :: String -> Int
getErrorPriority items = getPriority $ Set.toList $ c1 `Set.intersection` c2
    where (c1, c2) = both Set.fromList $ splitAt (length items `div` 2) items

getBadgePriority :: [String] -> Int
getBadgePriority (x:xs) = getPriority $ Set.toList $ foldr (Set.intersection . Set.fromList) (Set.fromList x) xs
getBadgePriority [] = error "Empty list."

part1 :: Solution
part1 = V . sumOn' getErrorPriority

part2 :: Solution
part2 = V . sumOn' getBadgePriority . chunksOf 3
