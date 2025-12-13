module Years.Year2018.Day03 (part1, part2) where

import Util.Util
import Data.List.Split
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> Map Int [Vec]
parseInput [] = Map.empty
parseInput (l:ls) = Map.insert i [(x, y) | x <- [px..px+dx-1], y <- [py..py+dy-1]] $ parseInput ls
    where ws = words l
          i = read $ tl $ hd ws
          [px, py] = map read $ splitOn "," $ ini $ ws !! 2
          [dx, dy] = map read $ splitOn "x" $ ws !! 3

getCounts :: Map Int [Vec] -> Map Vec Int
getCounts = Map.fromListWith (+) . map (, 1) . concat

part1 :: Solution
part1 = V . Map.size . Map.filter (> 1) . getCounts . parseInput

part2 :: Solution
part2 input = V $ fst $ Map.findMin $ Map.filter (all (`Set.member` noOverlap)) claims
    where claims = parseInput input
          noOverlap = Map.keysSet $ Map.filter (== 1) $ getCounts claims
