module Years.Year2020.Day11 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> (Map Vec Bool, Map Vec Bool)
parseInput input = (grid, Map.map (const False) $ Map.filter id grid)
    where grid = Map.fromList [((r, c), x == 'L') | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]

applyRules :: Int -> Map Vec [Vec] -> Map Vec Bool -> Int
applyRules maxOccAdj adjacents seats | seats == seats' = Map.size $ Map.filter id seats
                                     | otherwise = applyRules maxOccAdj adjacents seats'
    where seats' = Map.mapWithKey mapSeat seats
          mapSeat :: Vec -> Bool -> Bool
          mapSeat seat occ = let occAdj = filter (seats Map.!) $ adjacents Map.! seat
                             in case occ of
                                     False -> null occAdj
                                     True -> length occAdj <= maxOccAdj

part1 :: Solution
part1 input = V $ applyRules 3 adjacents seats
    where (_, seats) = parseInput input
          adjacents = Map.fromSet (\(r, c) -> [adj | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0),
                                                     let adj = (r + dr, c + dc), Map.member adj seats]) $ Map.keysSet seats


look :: Map Vec Bool -> Vec -> Vec -> Maybe Vec
look grid pos dir = case grid Map.!? pos' of
                         Nothing -> Nothing
                         Just True -> Just pos'
                         Just False -> look grid pos' dir
    where pos' = onBoth (+) pos dir

part2 :: Solution
part2 input = V $ applyRules 4 adjacents seats
    where (grid, seats) = parseInput input
          seatSet = Map.keysSet seats
          adjacents = Map.fromSet (\seat -> mapMaybe (look grid seat) [(dr, dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0)]) seatSet
