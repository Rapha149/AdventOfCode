module Years.Year2022.Day25 (part1) where

import Util.Util
import Data.Char
import Data.List.Extra

fromSnafu :: String -> Int
fromSnafu = foldl (\acc c -> acc * 5 + getDec c) 0
    where getDec :: Char -> Int
          getDec '-' = -1
          getDec '=' = -2
          getDec x = digitToInt x

toSnafu :: Int -> String
toSnafu 0 = ""
toSnafu x = toSnafu (quotient + if rest >= 3 then 1 else 0) ++ [char]
    where (quotient, rest) = x `divMod` 5
          char = case rest of
                      3 -> '='
                      4 -> '-'
                      _ -> intToDigit rest

part1 :: Solution
part1 = Msg . toSnafu . sumOn' fromSnafu
