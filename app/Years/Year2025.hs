module Years.Year2025 (selectDay) where

import Util.Util
import qualified Years.Year2025.Day01 as D01
import qualified Years.Year2025.Day02 as D02
import qualified Years.Year2025.Day03 as D03
import qualified Years.Year2025.Day04 as D04
import qualified Years.Year2025.Day05 as D05
import qualified Years.Year2025.Day06 as D06
import qualified Years.Year2025.Day07 as D07
import qualified Years.Year2025.Day08 as D08
import qualified Years.Year2025.Day09 as D09
import qualified Years.Year2025.Day10 as D10
import qualified Years.Year2025.Day11 as D11
import qualified Years.Year2025.Day12 as D12

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
    _ -> const $ Error "Unknown day/part combination."
