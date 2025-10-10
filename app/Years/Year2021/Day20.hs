module Years.Year2021.Day20 (part1, part2) where

import Util.Util
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

enhance :: Set Int -> Map Vec Bool -> Bool -> Map Vec Bool
enhance algorithm image outside = Map.fromSet enhancePixel pixels
    where pixels = foldMap (\(r, c) -> Set.fromList [(r + dr, c + dc) | dr <- [-1..1], dc <- [-1..1]]) $ Map.keysSet image
          enhancePixel :: Vec -> Bool
          enhancePixel (r, c) = let bin = [Map.findWithDefault outside (r + dr, c + dc) image | dr <- [-1..1], dc <- [-1..1]]
                                    dec = foldl (\acc v -> acc * 2 + fromEnum v) 0 bin
                                in Set.member dec algorithm

getResult :: Int -> [String] -> Int
getResult n (algInput:"":imgInput) | hd algInput == '#' && lst algInput == '#' = error "First and last chars in algorithm can't both be '#'."
                                   | otherwise = Map.size $ Map.filter id $ foldl (enhance algorithm) image outsides
    where algorithm = Set.fromList [i | (i, '#') <- zip [0..] algInput]
          image = Map.fromList [((r, c), x == '#') | (r, row) <- zip [0..] imgInput, (c, x) <- zip [0..] row]
          outsides | Set.notMember 0 algorithm = replicate n False
                     | otherwise = take n $ cycle [False, True]
getResult _ _ = error "Invalid input."

part1 :: Solution
part1 = V . getResult 2

part2 :: Solution
part2 = V . getResult 50
