module Years.Year2019.Day03 (part1, part2) where

import Util.Util
import Data.List.Split
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

getPath :: Int -> Vec -> [String] -> Map Vec Int
getPath _ _ [] = Map.empty
getPath _ _ ("":_) = error "Invalid input."
getPath len (x, y) ((dir:num):is) = Map.union (Map.fromList $ zip path [len+1..]) $ getPath (len + length path) (lst path) is
    where steps = read num
          path = case dir of
                      'U' -> map (x, ) [y+1 .. y+steps]
                      'D' -> map (x, ) [y-1, y-2 .. y-steps]
                      'R' -> map (, y) [x+1 .. x+steps]
                      'L' -> map (, y) [x-1, x-2 .. x-steps]
                      _ -> error "Invalid direction."

getIntersections :: [String] -> [(Vec, Int)]
getIntersections input = map (\pos -> (pos, path1 Map.! pos + path2 Map.! pos)) positions
    where [path1, path2] = map (getPath 0 (0, 0) . splitOn ",") input
          positions = Map.keys $ Map.intersection path1 path2

part1 :: Solution
part1 = V . minimum . map (uncurry (+) . both abs . fst) . getIntersections

part2 :: Solution
part2 = V . minimum . map snd . getIntersections
