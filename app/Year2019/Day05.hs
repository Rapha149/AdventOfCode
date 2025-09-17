module Year2019.Day05 (part1, part2) where

import Util
import Year2019.IntcodeComputer

part1 :: Solution
part1 = V . lst . fst . runIO [1] . parse

part2 :: Solution
part2 = V . hd . fst . runIO [5] . parse
