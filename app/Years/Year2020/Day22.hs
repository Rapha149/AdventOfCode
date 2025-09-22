module Years.Year2020.Day22 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> ([Int], [Int])
parseInput = pair . map (map read . tl) . split null

play1 :: [Int] -> [Int] -> [Int]
play1 xs [] = xs
play1 [] ys = ys
play1 (x:xs) (y:ys) | x > y = play1 (xs ++ [x, y]) ys
                    | otherwise = play1 xs (ys ++ [y, x])

getScore :: [Int] -> Int
getScore = sum . zipWith (*) [1..] . reverse

part1 :: Solution
part1 = V . getScore . uncurry play1 . parseInput


play2 :: Set (Int, Int) -> [Int] -> [Int] -> (Bool, [Int])
play2 _ xs [] = (True, xs)
play2 _ [] ys = (False, ys)
play2 seen (x:xs) (y:ys) | Set.member scores seen = (True, x:xs)
                         | winner = play2 seen' (xs ++ [x, y]) ys
                         | otherwise = play2 seen' xs (ys ++ [y, x])
    where scores = (getScore (x:xs), getScore (y:ys))
          seen' = Set.insert scores seen
          winner | length xs < x || length ys < y = x > y
                 | otherwise = fst $ play2 Set.empty (take x xs) (take y ys)

part2 :: Solution
part2 = V . getScore . snd . uncurry (play2 Set.empty) . parseInput
