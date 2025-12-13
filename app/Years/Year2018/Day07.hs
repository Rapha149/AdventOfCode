module Years.Year2018.Day07 (part1, part2) where

import Util.Util
import Data.Char
import Data.Maybe
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> Map Char (Set Char)
parseInput [] = Map.empty
parseInput (l:ls) = Map.insertWith Set.union b (Set.singleton a) $ parseInput ls
    where [a, b] = mapMaybe (\case [x] -> Just x; _ -> Nothing) $ words l

getOrder :: Map Char (Set Char) -> Set Char -> String
getOrder requirements ready = case Set.minView ready of
                                   Nothing -> ""
                                   Just (step, rest) -> let (newReady, requirements') = Map.partition null $ Map.map (Set.delete step) requirements
                                                        in step : getOrder requirements' (Set.union rest $ Map.keysSet newReady)

part1 :: Solution
part1 input = Msg $ getOrder requirements $ Set.filter (`Set.notMember` notReady) $ Set.unions requirements
    where requirements = parseInput input
          notReady = Map.keysSet requirements


getNewWorking :: Int -> Set Char -> Int -> (Set Char, [(Int, Set Char)])
getNewWorking _ ready 0 = (ready, [])
getNewWorking seconds ready available = case Set.minView ready of
                                             Nothing -> (Set.empty, [])
                                             Just (step, rest) -> second ((seconds + 1 + ord step - ord 'A', Set.singleton step) :) $ getNewWorking seconds rest (available - 1)

getTime :: Int -> Map Char (Set Char) -> Set Char -> Int -> Map Int (Set Char) -> Int -> Int
getTime seconds requirements ready n workers available | Map.member n workers = getTime seconds requirements' ready' n workers' (available + length steps)
    where steps = workers Map.! n
          (newReady, requirements') = Map.partition null $ Map.map (`Set.difference` steps) requirements
          ready' = Set.union ready $ Map.keysSet newReady
          workers' = Map.delete n workers
getTime seconds requirements ready n workers available | null ready && null workers = n
                                                       | null ready || available == 0 = getTime seconds requirements ready (n + 1) workers available
                                                       | otherwise = getTime seconds requirements ready' (n + 1) workers' (available - length newWorkers)
    where (ready', newWorkers) = getNewWorking (n + seconds) ready available
          workers' = Map.unionWith Set.union workers $ Map.fromListWith Set.union newWorkers

part2 :: Solution
part2 raw = V $ getTime seconds requirements ready 0 Map.empty workers
    where ((workers, seconds), input) = getExtraInts 2 pair (5, 60) raw
          requirements = parseInput input
          notReady = Map.keysSet requirements
          ready = Set.filter (`Set.notMember` notReady) $ Set.unions requirements
