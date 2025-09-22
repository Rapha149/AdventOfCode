module Years.Year2019.Day05 (part1, part2) where

import Util.Util
import Years.Year2019.IntcodeComputer

part1 :: Solution
part1 = V . lst . outputs . run . parseStateI [1]

part2 :: Solution
part2 = V . hd . outputs . run . parseStateI [5]
