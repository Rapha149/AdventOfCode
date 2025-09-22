module Years.Year2024.Day02 (part1, part2) where

import Util.Util

isReportSafe :: [Int] -> Bool
isReportSafe xs = isSafe xs
    where asc = hd xs < xs !! 1
          isSafe [] = True
          isSafe [_] = True
          isSafe (a:b:ys) = a /= b && asc == (a < b) && abs (a - b) <= 3 && isSafe (b:ys)

part1 :: Solution
part1 = V . length . filter isReportSafe . map (map read . words)

part2 :: Solution
part2 = V . length . filter (\xs -> any isReportSafe [take i xs ++ drop (i + 1) xs | i <- zipWith const [0..] xs]) . map (map read . words)
