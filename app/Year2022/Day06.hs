module Year2022.Day06 (part1, part2) where

import Util
import Data.List

getFirstMarker :: Int -> String -> Int
getFirstMarker n str | null [a | (a:x1) <- tails $ take n str, b <- x1, a == b] = n
                     | otherwise = 1 + getFirstMarker n (tl str)

part1 :: Solution
part1 = V . getFirstMarker 4 . hd

part2 :: Solution
part2 = V . getFirstMarker 14 . hd
