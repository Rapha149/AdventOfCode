module Year2022.Day02 (part1, part2) where

import Util
import Data.List
import Data.List.Extra
import Data.Maybe

getScore1 :: String -> Int
getScore1 [o, ' ', y] | opponent == you = 3 + you  + 1
                     | (opponent + 1) `mod` 3 == you = 6 + you + 1
                     | otherwise = you + 1
    where opponent = fromJust $ elemIndex o "ABC"
          you = fromJust $ elemIndex y "XYZ"
getScore1 _ = error "Invalid line."

part1 :: Solution
part1 = V . sumOn' getScore1


getScore2 :: String -> Int
getScore2 [o, ' ', r] = case r of
                             'X' -> (opponent + 2) `mod` 3 + 1
                             'Y' -> opponent + 4
                             'Z' -> (opponent + 1) `mod` 3 + 7
                             _ -> error "Invalid char."
    where opponent = fromJust $ elemIndex o "ABC"
getScore2 _ = error "Invalid line."

part2 :: Solution
part2 = V . sumOn' getScore2
