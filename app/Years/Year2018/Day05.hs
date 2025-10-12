module Years.Year2018.Day05 (part1, part2) where

import Util.Util
import Data.Char
import Data.List

reactUntilStable :: String -> String
reactUntilStable polymer | polymer == polymer' = polymer
                         | otherwise = reactUntilStable polymer'
    where polymer' = react polymer
          react :: String -> String
          react "" = ""
          react (a:b:xs) | toLower a == toLower b && isLower a /= isLower b = react xs
          react (x:xs) = x : react xs

part1 :: Solution
part1 = V . length . reactUntilStable . hd

part2 :: Solution
part2 input = V $ minimum [length $ reactUntilStable $ filter ((/= t) . toLower) reacted | t <- nub $ map toLower reacted]
    where reacted = reactUntilStable $ hd input
