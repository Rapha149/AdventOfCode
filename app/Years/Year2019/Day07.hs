module Years.Year2019.Day07 (part1, part2) where

import Util.Util
import Years.Year2019.IntcodeComputer
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

getOutput1 :: State -> Int -> [Int] -> Int
getOutput1 _ lastOutput [] = lastOutput
getOutput1 state lastOutput (phase:xs) = getOutput1 state (hd $ outputs $ run $ state { inputs = [phase, lastOutput] }) xs

part1 :: Solution
part1 input = let state = parseState input
              in V $ maximum $ map (getOutput1 state 0) $ permutations [0..4]


getOutput2 :: IntMap State -> Int -> [Int] -> Int
getOutput2 states idx lastOutputs | idx == 4 && state'.finished = lst state'.outputs
                                  | otherwise = getOutput2 (IM.insert idx (state' { outputs = [] }) states) ((idx + 1) `mod` 5) state'.outputs
    where state = states IM.! idx
          state' = run $ state { inputs = state.inputs ++ lastOutputs }

part2 :: Solution
part2 input = let state = parseState input
              in V $ maximum [getOutput2 states 0 [0] | phases <- permutations [5..9],
                                                        let states = IM.fromList $ zip [0..] $ map (\phase -> state { inputs = [phase] }) phases]
