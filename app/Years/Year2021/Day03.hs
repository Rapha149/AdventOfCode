module Years.Year2021.Day03 (part1, part2) where

import Util.Util
import Data.Char
import Data.List

mostCommon :: String -> Char
mostCommon s | length (filter (== '1') s) >= length (filter (== '0') s) = '1'
             | otherwise = '0'

notC :: Char -> Char
notC '0' = '1'
notC '1' = '0'
notC _ = error "Not a 0 or 1."

toDec :: String -> Int
toDec = foldl (\acc v -> acc * 2 + digitToInt v) 0

part1 :: Solution
part1 input = V $ toDec bits * toDec (map notC bits)
    where bits = map mostCommon $ transpose input


getRating2 :: (Char -> Char) -> [String] -> String
getRating2 _ [] = error "Empty numbers list."
getRating2 _ [x] = x
getRating2 f xs = common : getRating2 f (map tl $ filter ((== common) . hd) xs)
    where common = f $ mostCommon $ map hd xs

part2 :: Solution
part2 input = V $ toDec (getRating2 id input) * toDec (getRating2 notC input)
