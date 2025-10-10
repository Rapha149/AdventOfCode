module Years.Year2024.Day04 (part1, part2) where

import Util.Util
import Data.List
import Data.Function

diagonals :: Bool -> [String] -> [String]
diagonals anti rows = map (map snd) $ groupBy ((==) `on` fst) $ sortOn fst indexed
    where indexed :: [(Int, Char)]
          indexed = [((if anti then (-) else (+)) i j, x) | (i, row) <- zip [0..] rows, (j, x) <- zip [0..] row]

count :: [String] -> Int
count = sum . map (\s -> countLine s + countLine (reverse s))
    where countLine :: String -> Int
          countLine [] = 0
          countLine ('X':'M':'A':'S':cs) = 1 + countLine cs
          countLine (_:cs) = countLine cs

part1 :: Solution
part1 input = V $ count input + count (transpose input) + count (diagonals False input) + count (diagonals True input)


countX :: [String] -> Int
countX (l1:l2:l3:ls) = countXLines l1 l2 l3 + countXLines (reverse l1) (reverse l2) (reverse l3) + countX (l2:l3:ls)
    where countXLines :: String -> String -> String -> Int
          countXLines [] [] [] = 0
          countXLines ('M':c1:'S':cs1) (_:'A':c2:cs2) ('M':c3:'S':cs3) = 1 + countXLines (c1:'S':cs1) ('A':c2:cs2) (c3:'S':cs3)
          countXLines (_:cs1) (_:cs2) (_:cs3) = countXLines cs1 cs2 cs3
          countXLines _ _ _ = error "Number of characters don't match."
countX _  = 0

part2 :: Solution
part2 input = V $ countX input + countX (transpose input)
