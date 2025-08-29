module Year2022.Day10 (part1, part2) where

import Util
import Data.List.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

getCycleValues :: Int -> [String] -> [Int]
getCycleValues _ [] = []
getCycleValues x ("noop":is) = x : getCycleValues x is
getCycleValues x (i:is) = x : x : getCycleValues (x + read (drop 5 i)) is

part1 :: Solution
part1 = V . sumOn' (uncurry (*)) . filter (\(n, _) -> (n - 20) `mod` 40 == 0) . zip [1..] . getCycleValues 1


draw :: Int -> Map Int Int -> String
draw n sprites | Map.notMember n sprites = ""
               | otherwise = (if pixel == 0 then ('\n':) else id) (if abs (sprite - pixel) <= 1 then "#" else " ") ++ draw (n + 1) sprites
    where pixel = (n - 1) `mod` 40
          sprite = sprites Map.! n

part2 :: Solution
part2 = Msg . draw 1 . Map.fromList . zip [1..] . getCycleValues 1
