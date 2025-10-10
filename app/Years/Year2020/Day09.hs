module Years.Year2020.Day09 (part1, part2) where

import Util.Util
import Data.List
import Data.Tuple.Extra

parseInput :: [String] -> (Int, [Int])
parseInput = second (map read) . getExtra1 (isPrefixOf "len:") (read . drop 4) 25

firstInvalid :: Int -> [Int] -> Int
firstInvalid len xs | length xs <= len = error "No invalid number found."
                    | or [a + b == n | (a:bs) <- tails preamble, b <- bs] = firstInvalid len $ tl xs
                    | otherwise = n
    where (preamble, rest) = splitAt len xs
          n = hd rest

part1 :: Solution
part1 = V . uncurry firstInvalid . parseInput

part2 :: Solution
part2 input = V $ hd [minimum xs + maximum xs | i <- [0..n-1], l <- [1..n-i], let xs = take l $ drop i numbers, sum xs == invalid]
    where (len, numbers) = parseInput input
          invalid = firstInvalid len numbers
          n = length numbers
