module Years.Year2020.Day12 (part1, part2) where

import Util.Util
import Data.Tuple.Extra

data Instruction = Move Vec | Forward Int | Rotate Int

parseInput :: [String] -> [Instruction]
parseInput [] = []
parseInput ((c:v):ls) = (case c of
                              'N' -> Move (0, value)
                              'E' -> Move (value, 0)
                              'S' -> Move (0, -value)
                              'W' -> Move (-value, 0)
                              'F' -> Forward value
                              'L' -> Rotate $ 360 - value
                              'R' -> Rotate value
                              _ -> error "Invalid instruction.") : parseInput ls
    where value = read v
parseInput _ = error "Invalid instruction."

move :: Bool ->  [Instruction] -> Vec -> Vec -> Vec
move _ [] pos _ = pos
move waypoint (Move m:ms) pos dir | waypoint = move waypoint ms pos (onBoth (+) dir m)
                                  | otherwise = move waypoint ms (onBoth (+) pos m) dir
move waypoint (Forward f:ms) pos dir = move waypoint ms (onBoth (+) pos $ both (* f) dir) dir
move waypoint (Rotate r:ms) pos (dx, dy) = move waypoint ms pos $ case r of
                                                              90 -> (dy, -dx)
                                                              180 -> (-dx, -dy)
                                                              270 -> (-dy, dx)
                                                              _ -> error "Invalid degrees."

part1 :: Solution
part1 input = V $ uncurry (+) $ both abs $ move False (parseInput input) (0, 0) (1, 0)

part2 :: Solution
part2 input = V $ uncurry (+) $ both abs $ move True (parseInput input) (0, 0) (10, 1)
