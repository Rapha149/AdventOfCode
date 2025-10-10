module Years.Year2019.Day24 (part1, part2) where

import Util.Util
import Data.Foldable.Extra
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> Map Vec Bool
parseInput input = Map.fromList [((r, c), d == '#') | (r, row) <- zip [0..] input, (c, d) <- zip [0..] row]

firstRepeating :: Set Int -> Map Vec Bool -> Int
firstRepeating ratings bugs | Set.member rating ratings = rating
                            | otherwise = firstRepeating (Set.insert rating ratings) (Map.mapWithKey mapBug bugs)
    where rating = sum $ snd $ Map.mapAccum (\acc b -> (acc * 2, acc * fromEnum b)) 1 bugs
          mapBug :: Vec -> Bool -> Bool
          mapBug pos b = let count = length $ filter (\d -> Map.findWithDefault False (onBoth (+) pos d) bugs) [(1, 0), (-1, 0), (0, 1), (0, -1)]
                         in if b then count == 1 else count `elem` [1, 2]

part1 :: Solution
part1 = V . firstRepeating Set.empty . parseInput


getAdjacent :: Int -> Vec -> [(Int, Vec)]
getAdjacent layer (0, 0) = [(layer, (0, 1)), (layer, (1, 0)), (layer - 1, (1, 2)), (layer - 1, (2, 1))]
getAdjacent layer (0, 4) = [(layer, (0, 3)), (layer, (1, 4)), (layer - 1, (1, 2)), (layer - 1, (2, 3))]
getAdjacent layer (4, 0) = [(layer, (3, 0)), (layer, (4, 1)), (layer - 1, (3, 2)), (layer - 1, (2, 1))]
getAdjacent layer (4, 4) = [(layer, (3, 4)), (layer, (4, 3)), (layer - 1, (3, 2)), (layer - 1, (2, 3))]
getAdjacent layer (0, c) = [(layer, (0, c + 1)), (layer, (1, c)), (layer - 1, (1, 2)), (layer, (0, c - 1))]
getAdjacent layer (4, c) = [(layer, (4, c + 1)), (layer, (3, c)), (layer - 1, (3, 2)), (layer, (4, c - 1))]
getAdjacent layer (r, 0) = [(layer, (r + 1, 0)), (layer, (r, 1)), (layer - 1, (2, 1)), (layer, (r - 1, 0))]
getAdjacent layer (r, 4) = [(layer, (r + 1, 4)), (layer, (r, 3)), (layer - 1, (2, 3)), (layer, (r - 1, 4))]
getAdjacent layer (1, 2) = (layer, (0, 2)) : (layer, (1, 1)) : (layer, (1, 3)) : [(layer + 1, (0, c)) | c <- [0..4]]
getAdjacent layer (3, 2) = (layer, (4, 2)) : (layer, (3, 1)) : (layer, (3, 3)) : [(layer + 1, (4, c)) | c <- [0..4]]
getAdjacent layer (2, 1) = (layer, (2, 0)) : (layer, (1, 1)) : (layer, (3, 1)) : [(layer + 1, (r, 0)) | r <- [0..4]]
getAdjacent layer (2, 3) = (layer, (2, 4)) : (layer, (1, 3)) : (layer, (3, 3)) : [(layer + 1, (r, 4)) | r <- [0..4]]
getAdjacent layer pos = map ((layer, ) . onBoth (+) pos) [(1, 0), (-1, 0), (0, 1), (0, -1)]

bugCycle :: Map Int (Map Vec Bool) -> Map Int (Map Vec Bool)
bugCycle layers = Map.mapWithKey (Map.mapWithKey . mapBug) layers'
    where empty = Map.fromList [((r, c), False) | r <- [0..4], c <- [0..4], (r, c) /= (2, 2)]
          ((mn, _), (mx, _)) = (Map.findMin &&& Map.findMax) layers
          layers' = Map.insert (mn - 1) empty $ Map.insert (mx + 1) empty layers
          mapBug :: Int -> Vec -> Bool -> Bool
          mapBug layer pos b = let count = length $ filter (\(l, p) -> maybe False (Map.! p) $ layers Map.!? l) $ getAdjacent layer pos
                               in if b then count == 1 else count `elem` [1, 2]

part2 :: Solution
part2 raw = V $ sumOn' (Map.size . Map.filter id) layers
    where (minutes, input) = getExtraInt 200 raw
          layers = foldr ($) (Map.singleton 0 $ Map.delete (2, 2) $ parseInput input) $ replicate minutes bugCycle
