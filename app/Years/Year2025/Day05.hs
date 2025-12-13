module Years.Year2025.Day05 (part1, part2) where

import Util.Util
import Data.List.Extra

type Range = (Int, Int)

parseInput :: [String] -> ([Range], [Int])
parseInput input = (map (pair . map read . splitOn "-") ranges, map read available)
    where [ranges, available] = split null input

part1 :: Solution
part1 input = V $ length $ filter (\i -> any (\(a, b) -> i >= a && i <= b) ranges) available
    where (ranges, available) = parseInput input


countIds :: [Range] -> [Range] -> Int
countIds _ [] = 0
countIds seen (x:xs) = case removeOverlaps seen x of
                            Just (a, b) -> b - a + 1 + countIds ((a, b) : seen) xs
                            Nothing -> countIds seen xs
    where removeOverlaps :: [Range] -> Range -> Maybe Range
          removeOverlaps [] (a, b) = Just (a, b)
          removeOverlaps ((c, d):ys) (a, b) = let a' = if c <= a && a <= d then d + 1 else a
                                                  b' = if c <= b && b <= d then c - 1 else b
                                              in if a' <= b' then removeOverlaps ys (a', b') else Nothing

part2 :: Solution
part2 = V . countIds [] . sortOn (uncurry (-)) . fst . parseInput
