module Years.Year2025.Day11 (part1, part2) where

import Util.Util
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Device = String

parseInput :: [String] -> Map Device [Device]
parseInput [] = Map.empty
parseInput (l:ls) = case words l of
                         (i:os) -> Map.insert (ini i) os $ parseInput ls
                         _ -> error "Invalid input."

countPaths :: String -> Map Device [Device] -> Device -> Map Device Int -> Map Device Int
countPaths target graph pos cache | pos == target = Map.insert pos 1 cache
                                  | pos `Map.member` cache = cache
                                  | pos `Map.notMember` graph = Map.insert pos 0 cache
                                  | otherwise = Map.insert pos result cache'
    where next = graph Map.! pos
          cache' = foldr (countPaths target graph) cache next
          result = sum $ Map.restrictKeys cache' $ Set.fromList next

countPathsFromTo :: Device -> Device -> Map Device [Device] -> Int
countPathsFromTo source target graph = countPaths target graph source Map.empty Map.! source

part1 :: Solution
part1 = V . countPathsFromTo "you" "out" . parseInput

part2 :: Solution
part2 input = V $ svrDac * dacFft * fftOut + svrFft * fftDac * dacOut
    where graph = parseInput input
          svrDac = countPathsFromTo "svr" "dac" graph
          dacFft = countPathsFromTo "dac" "fft" graph
          fftOut = countPathsFromTo "fft" "out" graph
          svrFft = countPathsFromTo "svr" "fft" graph
          fftDac = countPathsFromTo "fft" "dac" graph
          dacOut = countPathsFromTo "dac" "out" graph
