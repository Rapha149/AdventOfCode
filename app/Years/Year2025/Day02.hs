module Years.Year2025.Day02 (part1, part2) where

import Util.Util
import Data.List.Split
import Text.Regex.PCRE

parseInput :: [String] -> [Int]
parseInput = concatMap parseRange . splitOn "," . concat
    where parseRange :: String -> [Int]
          parseRange s = case splitOn "-" s of
                              [a, b] -> [read a..read b]
                              _ -> error "Invalid input."

-- much faster than a regex for part 1
isInvalid :: Int -> Bool
isInvalid n = take mid s == drop mid s
    where s = show n
          mid = length s `div` 2

part1 :: Solution
part1 = V . sum . filter isInvalid . parseInput

part2 :: Solution
part2 = V . sum . filter ((=~ "^(.+)\\1+$") . show) . parseInput
