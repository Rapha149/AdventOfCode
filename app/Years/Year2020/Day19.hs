module Years.Year2020.Day19 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

data Rule = Chr Char | Rules [[Int]]

parseInput :: [String] -> (IntMap Rule, [String])
parseInput input = (IM.fromList [(read i, parseRule rule) | [i, rule] <- map (splitOn ": ") rules], messages)
    where [rules, messages] = split null input
          parseRule :: String -> Rule
          parseRule ['"', c, '"'] = Chr c
          parseRule str = Rules $ map (map read . words) $ splitOn " | " str

matches :: IntMap Rule -> [Int] -> String -> Bool
matches _ [] "" = True
matches _ [] _ = False
matches _ _ "" = False
matches rules (r:rs) str@(c:cs) = case rules IM.! r of
                                       Chr c' -> c == c' && matches rules rs cs
                                       Rules xs -> any (\rs' -> matches rules (rs' ++ rs) str) xs

part1 :: Solution
part1 input = V $ length $ filter (matches rules [0]) messages
    where (rules, messages) = parseInput input

part2 :: Solution
part2 input = V $ length $ filter (matches rules' [0]) messages
    where (rules, messages) = parseInput input
          rules' = IM.insert 8 (Rules [[42], [42, 8]]) $ IM.insert 11 (Rules [[42, 31], [42, 11, 31]]) rules
