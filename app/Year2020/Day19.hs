module Year2020.Day19 (part1, part2) where

import Util
import Data.Bifunctor
import Data.List.Extra
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

data Rule = Chr Char | Rules [[Int]]

parseInput :: [String] -> (IntMap Rule, [String])
parseInput input = (IM.fromList $ map (bimap read parseRule . pair . splitOn ": ") rules, messages)
    where (rules, messages) = pair $ split null input
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
part1 input = let (rules, messages) = parseInput input
              in V $ length $ filter (matches rules [0]) messages

part2 :: Solution
part2 input = let (rules, messages) = parseInput input
                  rules' = IM.insert 8 (Rules [[42], [42, 8]]) $ IM.insert 11 (Rules [[42, 31], [42, 11, 31]]) rules
              in V $ length $ filter (matches rules' [0]) messages
