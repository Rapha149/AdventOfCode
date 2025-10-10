module Years.Year2019.Day19 (part1, part2) where

import Util.Util
import Years.Year2019.IntcodeComputer
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

part1 :: Solution
part1 input = V $ sum [hd $ outputs $ run $ state { inputs = [x, y] } | x <- [0..49], y <- [0..49]]
    where state = parseState input


getSquare :: State -> (Double, Double) -> Set Vec -> Int -> Vec
getSquare state (minRatio, maxRatio) area y = case filter (\p@(_,y') -> y' + 100 == y && fits area' p) $ Set.toList area of
                                                   [] -> getSquare state (minRatio, maxRatio) area' $ y + 1
                                                   [p] -> p
                                                   _ -> error "Multiple solutions."
      where left = floor $ minRatio * fromIntegral y
            right = ceiling $ maxRatio * fromIntegral y
            area' = Set.union area $ Set.fromList [(x, y) | x <- [left..right], toEnum $ hd $ outputs $ run $ state { inputs = [x, y] }]

fits :: Set Vec -> Vec -> Bool
fits area (x, y) = Set.member (x + 99, y) area && Set.member (x, y + 99) area

part2 :: Solution
part2 input = let state = parseState input
                  smallArea = Map.fromListWith (++) [(fromIntegral y, [fromIntegral x]) | x <- [1..49], y <- [1..49], toEnum $ hd $ outputs $ run $ state { inputs = [x, y] }]
                  getRatio :: (forall a. Ord a => [a] -> a) -> Double
                  getRatio f = f $ Map.elems $ Map.mapWithKey (\y xs -> f xs / y) smallArea
                  ratios = (getRatio minimum - 0.1, getRatio maximum + 0.1)
                  (sx, sy) = getSquare state ratios Set.empty 0
              in V $ sx * 10000 + sy
