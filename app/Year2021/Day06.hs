module Year2021.Day06 (part1, part2) where

import Util
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

nextDay :: Map Int Int -> Map Int Int
nextDay fish = Map.insert 8 (Map.findWithDefault 0 0 fish) $ Map.mapKeysWith (+) (\case 0 -> 6; x -> x - 1) fish

getResult :: Int -> [String] -> Int
getResult days input = Map.foldr (+) 0 $ foldr ($) fish $ replicate days nextDay
    where fish = Map.fromListWith (+) $ map ((, 1) . read) $ splitOn "," $ hd input

part1 :: Solution
part1 = V . getResult 80

part2 :: Solution
part2 = V . getResult 256
