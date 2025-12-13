module Years.Year2019.Day04 (part1, part2) where

import Util.Util
import Data.List.Split
import qualified Data.Map.Strict as Map

countNumbers :: (Int -> Bool) -> Int -> Int -> Int -> Int -> Int
countNumbers valid _ _ 0 num = fromEnum $ any valid $ Map.fromListWith (+) $ map (, 1) $ show num
countNumbers valid mn mx tens num = sum [countNumbers valid mn mx (tens `div` 10) n | d <- [digit .. 9], let n = num * 10 + d, n >= mn' && n <= mx']
    where digit = num `mod` 10
          mn' = mn `div` tens
          mx' = mx `divCeil` tens

getResult :: (Int -> Bool) -> [String] -> Int
getResult valid [line] = countNumbers valid mn mx 100000 0
    where [mn, mx] = map read $ splitOn "-" line
getResult _ _ = error "Invalid input."

part1 :: Solution
part1 = V . getResult (>= 2)

part2 :: Solution
part2 = V . getResult (== 2)
