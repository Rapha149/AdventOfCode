module Years.Year2019.Day11 (part1, part2) where

import Util.Util
import Years.Year2019.IntcodeComputer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

paint :: Map Vec Bool -> Vec -> Vec -> State -> Map Vec Bool
paint colors tile (dx, dy) state | state'.status == Finished = colors'
                                 | otherwise = paint colors' (onBoth (+) tile dir') dir' $ state' { outputs = [] }
    where state' = run $ state { inputs = [maybe 0 fromEnum $ colors Map.!? tile] }
          [color, turn] = state'.outputs
          colors' = Map.insert tile (toEnum color) colors
          dir' = case turn of
                      0 -> (dy, -dx)
                      1 -> (-dy, dx)
                      _ -> error "Invalid output."

part1 :: Solution
part1 = V . Map.size . paint Map.empty (0, 0) (0, 1) . parseState

part2 :: Solution
part2 input = OCR painted
    where painted = Map.keysSet $ Map.filter id $ paint (Map.singleton (0, 0) True) (0, 0) (0, -1) $ parseState input
