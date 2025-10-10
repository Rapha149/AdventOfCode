module Years.Year2019.Day02 (part1, part2) where

import Util.Util
import Years.Year2019.IntcodeComputer
import qualified Data.IntMap.Strict as IM

setOneTwo :: Int -> Int -> State -> State
setOneTwo a b state@State {program} = state { program = IM.insert 1 a $ IM.insert 2 b program }

part1 :: Solution
part1 = V . (IM.! 0) . program . run . setOneTwo 12 2 . parseState

part2 :: Solution
part2 input = V $ hd [100 * noun + verb | noun <- [0..99], verb <- [0..99],
                                          program (run $ setOneTwo noun verb state) IM.! 0 == 19690720]
    where state = parseState input
