module Years.Year2020.Day16 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Range = (Int, Int)

parseInput :: [String] -> (Map String [Range], [Int], [[Int]])
parseInput input = (rules, yourTicket, nearbyTickets)
    where (ruleInput, yourInput, nearbyInput) = triple $ split null input
          rules = Map.fromList $ map (second (map (pair . map read . splitOn "-") . splitOn " or ") . pair . splitOn ": ") ruleInput
          yourTicket = map read $ splitOn "," $ yourInput !! 1
          nearbyTickets = map (map read . splitOn ",") $ tl nearbyInput

isInRanges :: [Range] -> Int -> Bool
isInRanges ranges n = any (\(a, b) -> n >= a && n <= b) ranges

part1 :: Solution
part1 input = V $ sum $ concatMap (filter (not . isInRanges (concat rules))) nearby
    where (rules, _, nearby) = parseInput input


findPositions :: Map String [Range] -> [(Int, Int)] -> Map String [Int] -> Map String Int
findPositions _ [] _ = error "No clear positions found."
findPositions rules ((field, value):xs) possible | null possible' = positions
                                                 | otherwise = Map.union positions $ findPositions rules xs possible'
    where impossible = Map.keys $ Map.filter (not . (`isInRanges` value)) rules
          (positions, possible') = getFound $ foldr (Map.adjust (delete field)) possible impossible
          getFound :: Map String [Int] -> (Map String Int, Map String [Int])
          getFound m = let found = Map.toList $ Map.map hd $ Map.filter ((== 1) . length) m
                           values = map snd found
                           m' = Map.filter (not . null) $ Map.map (\\ values) m
                       in if null found then (Map.empty, m) else first (Map.union $ Map.fromList found) $ getFound m'

part2 :: Solution
part2 input = V $ product $ Map.filterWithKey (const . isPrefixOf prefix) yourValues
    where (prefix, (rules, yours, nearby)) = second parseInput $ getExtra1 (notElem ':') id "departure" input
          valid = filter (all (isInRanges (concat rules))) nearby
          fields = concatMap (zip [0..]) valid
          initPositions = [0 .. length yours - 1]
          positions = findPositions rules fields (Map.map (const initPositions) rules)
          yourValues = Map.map (yours !!) positions
