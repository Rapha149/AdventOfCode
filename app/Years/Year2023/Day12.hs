module Years.Year2023.Day12 (part1, part2) where

import Util.Util
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type State = (Int, Int)

possibleCombinations :: String -> [Int] -> Int
possibleCombinations springs conditions = Map.foldr (+) 0 $ getPermutations springs (Map.singleton (0,0) 1) (length $ filter (/= '.') springs)
    where required = [sum $ drop i conditions | i <- [0..length conditions]]
          getPermutations :: String -> Map State Int -> Int -> Map State Int
          getPermutations [] states _ = states
          getPermutations (c:cs) states left = let newLeft = left - (if c == '.' then 0 else 1)
                                               in getPermutations cs (nextStates c states newLeft) newLeft
          nextStates :: Char -> Map State Int -> Int -> Map State Int
          nextStates c states left = Map.filterWithKey (\(grp, amount) _ -> left + amount >= required !! grp) $ Map.foldrWithKey (foldStates c) Map.empty states
          foldStates :: Char -> State -> Int -> Map State Int -> Map State Int
          foldStates c (grp, amount) perms = endCurrent . increaseCurrent
              where increaseCurrent | c /= '.' && grp < length conditions && amount < conditions !! grp = Map.insert (grp, amount + 1) perms
                                    | otherwise = id
                    endCurrent | c /= '#' && amount == 0 = Map.insertWith (+) (grp, 0) perms
                               | c /= '#' && amount == conditions !! grp = Map.insertWith (+) (grp + 1, 0) perms
                               | otherwise = id

part1 :: Solution
part1 = V . sum . map (uncurry possibleCombinations . (\[a, b] -> (a, map read $ splitOn "," b)) . words)

part2 :: Solution
part2 = V . sum . map (uncurry possibleCombinations . (\[a, b] -> (intercalate "?" $ replicate 5 a, concat $ replicate 5 $ map read $ splitOn "," b)) . words)
