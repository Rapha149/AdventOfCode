module Years.Year2024.Day05 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> (Map (Int, Int) Ordering, [[Int]])
parseInput input = (foldr parseRule Map.empty rules, map (map read . splitOn ",") pages)
    where [rules, pages] = split null input

parseRule :: String -> Map (Int, Int) Ordering -> Map (Int, Int) Ordering
parseRule rule = Map.insert (a,b) LT . Map.insert (b,a) GT
    where a = read $ take 2 rule
          b = read $ drop 3 rule

sortByRules :: Map (Int, Int) Ordering -> [Int] -> [Int]
sortByRules rules = sortBy (\a b -> Map.findWithDefault EQ (a,b) rules)

getMiddle :: [Int] -> Int
getMiddle xs = middle xs xs
    where middle (x:_) [] = x
          middle (x:_) [_] = x
          middle (_:as) (_:_:bs) = middle as bs
          middle [] _ = error "Empty list."

part1 :: Solution
part1 input = V $ sum $ map getMiddle $ filter (\xs -> xs == sortByRules rules xs) updates
    where (rules, updates) = parseInput input

part2 :: Solution
part2 input = V $ sum $ map (getMiddle . snd) $ filter (uncurry (/=)) $ map (\xs -> (xs, sortByRules rules xs)) updates
    where (rules, updates) = parseInput input
