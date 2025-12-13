module Years.Year2023.Day18 (part1, part2) where

import Util.Util
import Numeric

getCorners :: [(Char, Int)] -> [Vec]
getCorners = scanl step (0, 0)
    where step pos (dir, count) = let (dx, dy) = case dir of
                                                      'U' -> (0, -1)
                                                      'D' -> (0, 1)
                                                      'L' -> (-1, 0)
                                                      'R' -> (1, 0)
                                                      _ -> error "Unknown direction."
                                  in onBoth (+) pos (dx * count, dy * count)

shoelace :: [Vec] -> Int
shoelace corners = (`div` 2) $ abs $ sum $ zipWith (\(x0, y0) (x1, y1) -> x0 * y1 - x1 * y0) corners (tl corners ++ [hd corners])

movesToArea :: [(Char, Int)] -> Int
movesToArea moves = shoelace corners + sum (map snd moves) `div` 2 + 1
    where corners = getCorners moves

part1 :: Solution
part1 = V . movesToArea . map ((\([d]:c:_) -> (d, read c)) . words)

part2 :: Solution
part2 = V . movesToArea . map ((\(hex, dir) -> ("RDLU" !! read dir, fst $ hd $ readHex hex)) . splitAt 5 . take 6 . drop 2 . (!! 2) . words)
