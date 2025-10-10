module Years.Year2020.Day13 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

nextMultiple :: Int -> Int -> Int
nextMultiple a b = (a `divCeil` b) * b

part1 :: Solution
part1 input = V $ uncurry (*) $ second (subtract earliest) $ minimumOn snd $ map (id &&& nextMultiple earliest) buses
    where earliest = read $ hd input
          buses = map read $ filter (/= "x") $ splitOn "," $ input !! 1


align :: [(Int, Int)] -> Map Int Int -> Int -> Int -> Int
align buses timestamps time skip = case firstIncorrect of
                                        Nothing -> time
                                        Just i -> let timestamps' = Map.insert i time timestamps
                                                  in case timestamps Map.!? i of
                                                          Nothing -> align buses timestamps' (time + skip) skip
                                                          Just t -> let skip' = max skip (time - t)
                                                                    in align buses timestamps' (time + skip') skip'
    where firstIncorrect = findIndex (\(d, b) -> (time + d) `mod` b /= 0) buses

part2 :: Solution
part2 input = V $ align buses Map.empty 0 1
    where buses = map (second read) $ filter ((/= "x") . snd) $ zip [0..] $ splitOn "," $ input !! 1
