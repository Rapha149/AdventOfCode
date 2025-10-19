module Years.Year2018.Day12 (part1, part2) where

import Util.Util
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type Rule = ([Bool], Bool)

parseInput :: [String] -> (Set Int, [Rule])
parseInput (stateInput:"":ruleInput) = (state, map (parseRule . words) ruleInput)
    where state = Set.fromList [i | (i, '#') <- zip [0..] $ lst $ words stateInput]
          parseRule :: [String] -> ([Bool], Bool)
          parseRule [conds, "=>", res] = (map (== '#') conds, res == "#")
          parseRule _ = error "Invalid rule input."
parseInput _ = error "Invalid input."

applyRules :: [Rule] -> Set Int -> Set Int
applyRules rules plants = Set.fromList [i | i <- [mn..mx], let env = [Set.member j plants | j <- [i-2..i+2]],
                                           maybe False snd $ find ((== env) . fst) rules]
    where mn = Set.findMin plants - 2
          mx = Set.findMax plants + 2

part1 :: Solution
part1 input = V $ sum $ foldr ($) plants $ replicate 20 (applyRules rules)
    where (plants, rules) = parseInput input


getResult :: [Rule] -> Int -> Set Int -> Int -> Int -> Int
getResult rules gens plants lastDiff equalDiffs | equalDiffs == 100 = s + gens * diff
                                                | otherwise = getResult rules (gens - 1) plants' diff $ if diff == lastDiff then equalDiffs + 1 else 0
    where plants' = applyRules rules plants
          s = sum plants
          diff = sum plants' - s

part2 :: Solution
part2 input = V $ getResult rules 50000000000 plants minBound 0
    where (plants, rules) = parseInput input
