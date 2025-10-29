module Years.Year2018.Day17 (part1, part2) where

import Util.Util
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Tile = Wall | Flowing | Settled deriving (Show, Eq)

parseInput :: [String] -> Map Vec Tile
parseInput [] = Map.empty
parseInput (('x':'=':l):ls) = Map.fromList [((x, y), Wall) | y <- [y1..y2]] `Map.union` parseInput ls
    where (xInput, yInput) = pair $ splitOn ", "l
          x = read xInput
          (y1, y2) = pair $ map read $ splitOn ".." $ drop 2 yInput
parseInput (('y':'=':l):ls) = Map.fromList [((x, y), Wall) | x <- [x1..x2]] `Map.union` parseInput ls
    where (yInput, xInput) = pair $ splitOn ", " l
          y = read yInput
          (x1, x2) = pair $ map read $ splitOn ".." $ drop 2 xInput
parseInput _ = error "Invalid input"

flow :: Int -> Map Vec Tile -> Vec -> Map Vec Tile
flow maxY grid (x, y) | y >= maxY = grid
                      | gridLR Map.! (x, y + 1) == Flowing = gridLR
                      | otherwise = case liftA2 Map.union (getSettledWater x 1) (getSettledWater x (-1)) of
                                         Nothing -> gridLR
                                         Just m -> Map.union m gridLR
    where gridD = flowIfEmpty grid (x, y + 1)
          gridLR | maybe True (== Flowing) $ gridD Map.!? (x, y + 1) = gridD
                 | otherwise = flowIfEmpty (flowIfEmpty gridD (x - 1, y)) (x + 1, y)
          flowIfEmpty :: Map Vec Tile -> Vec -> Map Vec Tile
          flowIfEmpty grid' pos | pos `Map.notMember` grid' = flow maxY (Map.insert pos Flowing grid') pos
                                | otherwise = grid'
          getSettledWater :: Int -> Int -> Maybe (Map Vec Tile)
          getSettledWater x' dx = case gridLR Map.!? (x', y) of
                                       Nothing -> Nothing
                                       Just Wall -> Just Map.empty
                                       _ -> Map.insert (x', y) Settled <$> getSettledWater (x' + dx) dx

getWaterLocations :: [String] -> Map Vec Tile
getWaterLocations input = Map.filterWithKey (\(_, y) t -> y >= minY && t /= Wall) $ flow maxY grid (500, 0)
    where grid = parseInput input
          (_, (minY, maxY)) = getBounds $ Map.keys grid

part1 :: Solution
part1 = V . length . getWaterLocations

part2 :: Solution
part2 = V . length . Map.filter (== Settled) . getWaterLocations
