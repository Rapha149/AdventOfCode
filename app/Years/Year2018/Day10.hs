module Years.Year2018.Day10 (part1, part2) where

import Util.Util
import Text.Regex.TDFA
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> [(Vec, Vec)]
parseInput [] = []
parseInput (l:ls) = both pair (splitAt 2 ints) : parseInput ls
    where ints = map read $ tl $ getAllTextSubmatches $ l =~ "^position=< *(-?[0-9]+), *(-?[0-9]+)> velocity=< *(-?[0-9]+), *(-?[0-9]+)>$"

move :: [(Vec, Vec)] -> [(Vec, Vec)]
move = map (\(pos, delta) -> (onBoth (+) pos delta, delta))

moveUntilMessage :: [(Vec, Vec)] -> (Int, Set Vec)
moveUntilMessage points | allConnected = (0, set)
                        | otherwise = first (+ 1) $ moveUntilMessage (move points)
    where set = Set.fromList $ map fst points
          allConnected = and [or [Set.member (x + dx, y + dy) set | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)] | ((x, y), _) <- points]

part1 :: Solution
part1 = OCR . snd . moveUntilMessage . parseInput

part2 :: Solution
part2 = V . fst . moveUntilMessage . parseInput
