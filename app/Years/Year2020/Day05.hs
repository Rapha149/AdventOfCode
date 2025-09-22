module Years.Year2020.Day05 (part1, part2) where

import Util.Util
import Data.List

getSeatId :: String -> Int
getSeatId seat = r * 8 + c
    where (row, column) = splitAt 7 seat
          r = getNumber (map (== 'F') row) 0 127
          c = getNumber (map (== 'L') column) 0 7
          getNumber :: [Bool] -> Int -> Int -> Int
          getNumber [] low _ = low
          getNumber (b:bs) low high = let mid = (low + high) `div` 2
                                      in if b then getNumber bs low mid
                                              else getNumber bs (mid + 1) high

part1 :: Solution
part1 = V . maximum . map getSeatId


findEmpty :: [Int] -> Int
findEmpty [] = error "Empty list."
findEmpty [_] = error "No empty seat between two seats found."
findEmpty (a:b:xs) | a + 1 == b = findEmpty (b:xs)
                   | otherwise = a + 1

part2 :: Solution
part2 = V . findEmpty . sort . map getSeatId
