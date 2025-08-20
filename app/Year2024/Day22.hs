module Year2024.Day22 (part1, part2) where

import Util
import Data.Bits
import Data.List.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

generateSecretNumbers :: Int -> Int -> [Int]
generateSecretNumbers 0 _ = []
generateSecretNumbers i n = next : generateSecretNumbers (i - 1) next
    where step :: (Int -> Int) -> Int -> Int
          step f n' = (f n' `xor` n') `mod` 16777216
          next = step (* 2048) $ step (`div` 32) $ step (* 64) n

getPricesBySequence :: [Int] -> Map (Int, Int, Int, Int) Int
getPricesBySequence (a:b:c:d:e:xs) = Map.insert (b - a, c - b, d - c, e - d) e $ getPricesBySequence (b:c:d:e:xs)
getPricesBySequence _ = Map.empty

part1 :: Solution
part1 = V . sumOn' (lst . generateSecretNumbers 2000 . read)

part2 :: Solution
part2 = V . maximum . Map.elems . foldr (Map.unionWith (+) . getPricesBySequence . map (`mod` 10) . generateSecretNumbers 2000 . read) Map.empty
