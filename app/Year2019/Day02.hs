module Year2019.Day02 (part1, part2) where

import Util
import Year2019.IntcodeComputer

setOneTwo :: Int -> Int -> [Int] -> [Int]
setOneTwo b c (a:_:_:xs) = a : b : c : xs
setOneTwo _ _ _ = error "Invalid input."

part1 :: Solution
part1 = V . hd . run . setOneTwo 12 2 . parse

part2 :: Solution
part2 input = let program = parse input
              in V $ hd [100 * noun + verb | noun <- [0..99], verb <- [0..99],
                                             hd (run $ setOneTwo noun verb program) == 19690720]
