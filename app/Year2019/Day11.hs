module Year2019.Day11 (part1, part2) where

import Util
import Year2019.IntcodeComputer
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

paint :: Map Vec Bool -> Vec -> Vec -> State -> Map Vec Bool
paint colors tile (dx, dy) state | state'.finished = colors'
                                 | otherwise = paint colors' (onBoth (+) tile dir') dir' $ state' { outputs = [] }
    where state' = run $ state { inputs = [maybe 0 fromEnum $ colors Map.!? tile] }
          (color, turn) = pair state'.outputs
          colors' = Map.insert tile (toEnum color) colors
          dir' = case turn of
                      0 -> (-dy, dx)
                      1 -> (dy, -dx)
                      _ -> error "Invalid output."

part1 :: Solution
part1 = V . Map.size . paint Map.empty (0, 0) (0, 1) . parseState


toString :: Set Vec -> [[Vec]] -> String
toString _ [] = ""
toString painted ([]:ys) = '\n' : toString painted ys
toString painted ((pos:xs):ys) = (if Set.member pos painted then '#' else ' ') : toString painted (xs:ys)

part2 :: Solution
part2 input = let painted = Map.keysSet $ Map.filter id $ paint (Map.singleton (0, 0) True) (0, 0) (0, 1) $ parseState input
                  ((minX, maxX), (minY, maxY)) = getBounds $ Set.toList painted
              in Msg $ toString painted [[(x, y) | x <- [minX..maxX]] | y <- [maxY, maxY-1 .. minY]]
