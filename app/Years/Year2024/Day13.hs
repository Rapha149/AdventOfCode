module Years.Year2024.Day13 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Numeric.LinearAlgebra

getMinTokens :: Double -> Int -> (String, String, String) -> Int
getMinTokens resAdd limit (bA, bB, prize) | isNothing result ||
                                                abs (aD - fromIntegral a) > 1e-4 || abs (bD - fromIntegral b) > 1e-4 ||
                                                limit > 0 && (a > limit || b > limit) = 0
                                          | otherwise = a * 3 + b
    where left = (2><2) $ (\((aX, aY), (bX, bY)) -> [aX, bX, aY, bY]) $ both (pair . map (read . drop 2) . splitOn ", " . drop 10) (bA, bB)
          right = (2><1) $ map ((+ resAdd) . read . drop 2) $ splitOn ", " $ drop 7 prize
          result = linearSolve left right
          (aD, bD) = pair $ concat $ toLists $ fromJust result
          (a, b) = both round (aD, bD)

part1 :: Solution
part1 = V . sum . map (getMinTokens 0.0 100 . triple) . split null

part2 :: Solution
part2 = V . sum . map (getMinTokens 10000000000000.0 0 . triple) . split null
