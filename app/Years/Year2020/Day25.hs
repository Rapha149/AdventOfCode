module Years.Year2020.Day25 (part1) where

import Util.Util

transformStep :: Int -> Int -> Int
transformStep subject n = (subject * n) `mod` 20201227

findLoopSize :: Int -> Int -> Int
findLoopSize publicKey n | n == publicKey = 0
                         | otherwise = 1 + findLoopSize publicKey (transformStep 7 n)

part1 :: Solution
part1 input = V $ foldr ($) 1 $ replicate loopSize (transformStep door)
    where [card, door] = map read input
          loopSize = findLoopSize card 1
