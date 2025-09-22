module Years.Year2020.Day07 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> Map String (Map String Int)
parseInput [] = Map.empty
parseInput (l:ls) = Map.insert key bags $ parseInput ls
    where (key, contains) = pair $ splitOn " bags contain " l
          bags | contains == "no other bags." = Map.empty
               | otherwise = Map.fromList $ map ((\ws -> (unwords $ ini $ tl ws, read $ hd ws)) . words) $ splitOn ", " contains

countBagsContaining :: Map String (Set String) -> [String] -> Int
countBagsContaining _ [] = 0
countBagsContaining bags (b:bs) = length containing + countBagsContaining bags' (containing ++ bs)
    where containing = Map.keys $ Map.filter (Set.member b) bags
          bags' = Map.withoutKeys bags $ Set.fromList containing

part1 :: Solution
part1 = V . (`countBagsContaining` ["shiny gold"]) . Map.map Map.keysSet . parseInput


countContainedBags :: Map String (Map String Int) -> String -> Int
countContainedBags bags bag = Map.foldrWithKey (\b c -> (+) (c * countContainedBags bags b)) (sum contained) contained
    where contained = bags Map.! bag

part2 :: Solution
part2 = V . (`countContainedBags` "shiny gold") . parseInput
