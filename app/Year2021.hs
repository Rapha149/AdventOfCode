module Year2021 (selectDay) where

import Util
import qualified Year2021.Day01 as D01
import qualified Year2021.Day02 as D02
import qualified Year2021.Day03 as D03
import qualified Year2021.Day04 as D04
import qualified Year2021.Day05 as D05
import qualified Year2021.Day06 as D06
import qualified Year2021.Day07 as D07
import qualified Year2021.Day08 as D08
import qualified Year2021.Day09 as D09
import qualified Year2021.Day10 as D10
import qualified Year2021.Day11 as D11
import qualified Year2021.Day12 as D12
import qualified Year2021.Day13 as D13
import qualified Year2021.Day14 as D14
import qualified Year2021.Day15 as D15
import qualified Year2021.Day16 as D16
import qualified Year2021.Day17 as D17
import qualified Year2021.Day18 as D18
import qualified Year2021.Day19 as D19
import qualified Year2021.Day20 as D20
import qualified Year2021.Day21 as D21
import qualified Year2021.Day22 as D22
import qualified Year2021.Day23 as D23
import qualified Year2021.Day24 as D24
import qualified Year2021.Day25 as D25

selectDay :: Int -> Int -> Solution
selectDay day part = case (day, part) of
    (1, 1) -> D01.part1
    (1, 2) -> D01.part2
    (2, 1) -> D02.part1
    (2, 2) -> D02.part2
    (3, 1) -> D03.part1
    (3, 2) -> D03.part2
    (4, 1) -> D04.part1
    (4, 2) -> D04.part2
    (5, 1) -> D05.part1
    (5, 2) -> D05.part2
    (6, 1) -> D06.part1
    (6, 2) -> D06.part2
    (7, 1) -> D07.part1
    (7, 2) -> D07.part2
    (8, 1) -> D08.part1
    (8, 2) -> D08.part2
    (9, 1) -> D09.part1
    (9, 2) -> D09.part2
    (10, 1) -> D10.part1
    (10, 2) -> D10.part2
    (11, 1) -> D11.part1
    (11, 2) -> D11.part2
    (12, 1) -> D12.part1
    (12, 2) -> D12.part2
    (13, 1) -> D13.part1
    (13, 2) -> D13.part2
    (14, 1) -> D14.part1
    (14, 2) -> D14.part2
    (15, 1) -> D15.part1
    (15, 2) -> D15.part2
    (16, 1) -> D16.part1
    (16, 2) -> D16.part2
    (17, 1) -> D17.part1
    (17, 2) -> D17.part2
    (18, 1) -> D18.part1
    (18, 2) -> D18.part2
    (19, 1) -> D19.part1
    (19, 2) -> D19.part2
    (20, 1) -> D20.part1
    (20, 2) -> D20.part2
    (21, 1) -> D21.part1
    (21, 2) -> D21.part2
    (22, 1) -> D22.part1
    (22, 2) -> D22.part2
    (23, 1) -> D23.part1
    (23, 2) -> D23.part2
    (24, 1) -> D24.part1
    (24, 2) -> D24.part2
    (25, 1) -> D25.part1
    _ -> const $ Error "Unknown day/part combination."
