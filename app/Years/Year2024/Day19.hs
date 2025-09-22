module Years.Year2024.Day19 (part1, part2) where

import Util.Util
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

isPossible :: [String] -> [String] -> String -> Bool
isPossible _ [] _ = False
isPossible _ _ "" = True
isPossible towels (t:ts) design = (t `isPrefixOf` design && isPossible towels towels (drop (length t) design)) || isPossible towels ts design

countCombinations :: [String] -> [String] -> Map String Int -> String -> String -> Int
countCombinations _ [] cache design [] = cache Map.! design
countCombinations towels [] cache design (d:ds)= countCombinations towels towels cache (d:design) ds
countCombinations towels (t:ts) cache design ds = countCombinations towels ts (Map.insertWith (+) design combos cache) design ds
    where combos = if t `isPrefixOf` design then cache Map.! drop (length t) design else 0

getResult :: ([String] -> String -> Int) -> [String] -> Int
getResult f input = sum $ map (f towels) $ drop 2 input
    where towels = splitOn ", " $ hd input

part1 :: Solution
part1 = V . getResult (\ts -> fromEnum . isPossible ts ts)

part2 :: Solution
part2 = V . getResult (\ts -> countCombinations ts [] (Map.singleton "" 1) "" . reverse)
