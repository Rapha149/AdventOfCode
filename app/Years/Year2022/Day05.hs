module Years.Year2022.Day05 (part1, part2) where

import Util.Util
import Data.Char
import Data.List.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

move :: ([Char] -> [Char]) -> Map Int [Char] -> [Int] -> Map Int [Char]
move order stacks [n, s, t] = Map.adjust (drop n) s $ Map.adjust (order (take n $ stacks Map.! s) ++) t stacks
move _ _ _ = error "Invalid input."

getResult :: ([Char] -> [Char]) -> [String] -> String
getResult order input = map hd $ Map.elems $ foldl (move order) stacks moves
    where [stackInput, moveInput] = split null input
          stacks = Map.fromList $ zip [1..] [ini $ dropWhile isSpace x | (i, x) <- zip [0 :: Int ..] $ transpose stackInput, i `mod` 4 == 1]
          moves = map (\m -> [read x | (i, x) <- zip [0 :: Int ..] $ words m, odd i]) moveInput

part1 :: Solution
part1 = Msg . getResult reverse

part2 :: Solution
part2 = Msg . getResult id
