module Years.Year2020.Day01 (part1, part2) where

import Util.Util
import Data.List

part1 :: Solution
part1 input = let numbers = map read input
              in V $ hd [a * b | (a:bs) <- tails numbers, b <- bs, a + b == 2020]

part2 :: Solution
part2 input = let numbers = map read input
              in V $ hd [a * b * c | (a:bs) <- tails numbers, (b:cs) <- tails bs, c <- cs, a + b + c == 2020]
