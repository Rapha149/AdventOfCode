module Years.Year2018.Day01 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set

part1 :: Solution
part1 = V . sumOn' (read . (\case ('+':xs) -> xs; xs -> xs))


firstDuplicate :: Set Int -> Int -> [Int] -> Int
firstDuplicate _ _ [] = error "Empty list."
firstDuplicate history n (x:xs) | Set.member n' history = n'
                                | otherwise = firstDuplicate (Set.insert n' history) n' xs
    where n' = n + x

part2 :: Solution
part2 = V . firstDuplicate Set.empty 0 . cycle . map (read . (\case ('+':xs) -> xs; xs -> xs))
