module Year2024.Day07 (part1, part2) where

import Util
import Data.List.Split
import Data.Bifunctor
import Control.Monad

type Operator = Int -> Int -> Int

isPossible :: [Operator] -> (Int, [Int]) -> Bool
isPossible operators (target, numbers) = any ((== target) . calc . zip (reverse numbers) . (++ [const])) $ replicateM (length numbers - 1) operators
    where calc :: [(Int, Operator)] -> Int
          calc [] = error "Empty list."
          calc [(x, _)] = x
          calc ((x, op):xs) = x `op` calc xs

sumPossible :: [Operator] -> [String] -> Int
sumPossible operators = sum . map fst . filter (isPossible operators) . map (bimap read (map read . words) . tuple . splitOn ":")

part1 :: Solution
part1 = V . sumPossible [(+), (*)]

part2 :: Solution
part2 = V . sumPossible [(+), (*), \a b -> read $ show b ++ show a] -- numbers get reversed
