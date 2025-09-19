module Year2019.Day08 (part1, part2) where

import Util
import Data.Char
import Data.List.Extra
import Data.Tuple.Extra

part1 :: Solution
part1 = V . uncurry (*) . both length . (filter (== 1) &&& filter (== 2)) . minimumOn (length . filter (== 0)) . chunksOf 150 . map digitToInt . hd

part2 :: Solution
part2 = Msg . unlines . chunksOf 25 . map ((\case 1 -> '#'; _ -> ' ') . hd . filter (/= 2)) . transpose . chunksOf 150 . map digitToInt . hd
