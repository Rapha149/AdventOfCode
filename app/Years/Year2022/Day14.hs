module Years.Year2022.Day14 (part1, part2) where

import Util.Util
import Data.List
import Data.List.Split
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> (Set Vec, Int)
parseInput input = (obstacles, maximum $ map snd $ Set.toList obstacles)
    where parsePath :: [Vec] -> Set Vec
          parsePath [] = error "Empty path."
          parsePath [_] = Set.empty
          parsePath ((x1,y1):(x2,y2):ps) = foldr Set.insert (parsePath ((x2,y2):ps)) [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
          obstacles = foldr (Set.union . parsePath . map (pair . map read . splitOn ",") . splitOn " -> ") Set.empty input

countUntilAbyss :: Set Vec -> Int -> Int
countUntilAbyss obstacles maxY = case fall (500, 0) of
                                      Just p -> 1 + countUntilAbyss (Set.insert p obstacles) maxY
                                      Nothing -> 0
    where fall :: Vec -> Maybe Vec
          fall p@(_,y) | y > maxY = Nothing
                       | otherwise = maybe (Just p) fall $ find (`Set.notMember` obstacles) $ map (onBoth (+) p) [(0, 1), (-1, 1), (1, 1)]

countUntilTop :: Set Vec -> Int -> Int
countUntilTop obstacles floorY | pos == (500, 0) = 1
                               | otherwise = 1 + countUntilTop (Set.insert pos obstacles) floorY
    where isFree :: Vec -> Bool
          isFree p@(_,y) = y /= floorY && Set.notMember p obstacles
          fall :: Vec -> Vec
          fall p = maybe p fall $ find isFree $ map (onBoth (+) p) [(0, 1), (-1, 1), (1, 1)]
          pos = fall (500, 0)

part1 :: Solution
part1 = V . uncurry countUntilAbyss . parseInput

part2 :: Solution
part2 = V . uncurry countUntilTop . second (+2) . parseInput
