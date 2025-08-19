module Year2023.Day01 (part1, part2) where

import Util
import Data.Char

part1 :: Solution
part1 = V . sum . map (read . (\xs -> [hd xs, lst xs]) . filter isDigit)

part2 :: Solution
part2 = V . sum . map ((\xs -> 10 * hd xs + lst xs) . getNumbers)
    where getNumbers :: String -> [Int]
          getNumbers [] = []
          getNumbers ('z':'e':'r':'o':cs) = 0 : getNumbers ('o':cs)
          getNumbers ('o':'n':'e':cs) = 1 : getNumbers ('e':cs)
          getNumbers ('t':'w':'o':cs) = 2 : getNumbers ('o':cs)
          getNumbers ('t':'h':'r':'e':'e':cs) = 3 : getNumbers ('e':cs)
          getNumbers ('f':'o':'u':'r':cs) = 4 : getNumbers ('r':cs)
          getNumbers ('f':'i':'v':'e':cs) = 5 : getNumbers ('e':cs)
          getNumbers ('s':'i':'x':cs) = 6 : getNumbers ('x':cs)
          getNumbers ('s':'e':'v':'e':'n':cs) = 7 : getNumbers ('n':cs)
          getNumbers ('e':'i':'g':'h':'t':cs) = 8 : getNumbers ('t':cs)
          getNumbers ('n':'i':'n':'e':cs) = 9 : getNumbers ('e':cs)
          getNumbers (c:cs) | isDigit c = digitToInt c : getNumbers cs
                            | otherwise = getNumbers cs
