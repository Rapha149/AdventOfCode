{-# LANGUAGE MultilineStrings #-}
module Years.Year2019.Day21 (part1, part2) where

import Util.Util
import Years.Year2019.IntcodeComputer
import Data.Char

springscript1 :: String
springscript1 = """
    NOT B J
    NOT C T
    OR T J
    AND D J
    NOT A T
    OR T J
    WALK

    """

part1 :: Solution
part1 = V . lst . outputs . run . parseStateI (map ord springscript1)


springscript2 :: String
springscript2 = """
    NOT B J
    NOT C T
    OR T J
    AND D J
    AND H J
    NOT A T
    OR T J
    RUN

    """

part2 :: Solution
part2 = V . lst . outputs . run . parseStateI (map ord springscript2)
