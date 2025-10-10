module Years.Year2024.Day03 (part1, part2) where

import Util.Util
import Text.Regex.TDFA

parse :: String -> Int
parse str | length groups == 2 = read (hd groups) * read (lst groups) + parse next
           | otherwise = 0
    where (_, _, next, groups) = str =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: (String, String, String, [String])

part1 :: Solution
part1 = V . parse . concat


enabled :: Bool -> String -> String
enabled _ [] = []
enabled True ('d':'o':'n':'\'':'t':'(':')':cs) = enabled False cs
enabled False ('d':'o':'(':')':cs) = enabled True cs
enabled True (c:cs) = c : enabled True cs
enabled False (_:cs) = enabled False cs

part2 :: Solution
part2 = V . parse . enabled True . concat
