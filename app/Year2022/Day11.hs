module Year2022.Day11 (part1, part2) where

import Util
import Data.Ord
import Data.List.Extra
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Monkey = M { op :: Int -> Int, test :: Int, ifTrue :: Int, ifFalse :: Int }
type Items = Map Int [Int]

parseInput :: [String] -> ([(Int, Monkey)], Items)
parseInput input = (zip [0..] $ map parseMonkey monkeys, startItems)
    where monkeys = split null input
          startItems = Map.fromList $ zip [0..] $ map (map read . splitOn ", " . drop 18 . (!! 1)) monkeys
          parseMonkey :: [String] -> Monkey
          parseMonkey ls = let (sym, num) = pair $ drop 4 $ words $ ls !! 2
                               op = if num == "old" then case sym of "+" -> (*2); "*" -> (^ (2 :: Int)); _ -> error "Unknown operation."
                                                    else (case sym of "+" -> (+); "*" -> (*); _ -> error "Unknown operation.") $ read num
                               (test, ifTrue, ifFalse) = triple $ map (read . lst . words . (!!) ls) [3,4,5]
                           in M {..}

processRound :: (Int -> Int) -> [(Int, Monkey)] -> Items -> (Items, Map Int Int)
processRound _ [] items = (items, Map.empty)
processRound relief ((i, M {..}):ms) items = second (Map.insert i $ length itemList) $ processRound relief ms $ Map.insert i [] $ foldr processItem items itemList
    where processItem :: Int -> Items -> Items
          processItem item = let i' = relief $ op item
                                 m = if i' `mod` test == 0 then ifTrue else ifFalse
                             in Map.adjust (i':) m
          itemList = items Map.! i

getInspectionCounts :: Int -> (Int -> Int) -> [(Int, Monkey)] -> Items -> Map Int Int
getInspectionCounts 0 _ _ _ = Map.empty
getInspectionCounts r relief monkeys items = Map.unionWith (+) counts $ getInspectionCounts (r - 1) relief monkeys items'
    where (items', counts) = processRound relief monkeys items

getMonkeyBusiness :: Int -> (Int -> Int) -> ([(Int, Monkey)], Items) -> Int
getMonkeyBusiness rounds relief = uncurry (*) . pair . take 2 . sortOn Down . Map.elems . uncurry (getInspectionCounts rounds relief)

part1 :: Solution
part1 = V . getMonkeyBusiness 20 (`div` 3) . parseInput

part2 :: Solution
part2 input = let parsed@(monkeys,_) = parseInput input
                  multiple = foldr (lcm . test . snd) 1 monkeys
              in V $ getMonkeyBusiness 10000 (`mod` multiple) parsed
