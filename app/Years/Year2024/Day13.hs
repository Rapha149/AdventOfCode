module Years.Year2024.Day13 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.List.Extra
import Numeric.LinearAlgebra

getMinTokens :: Double -> Int -> [String] -> Int
getMinTokens resAdd limit [bA, bB, prize] | isNothing result ||
                                                abs (aD - fromIntegral a) > 1e-4 || abs (bD - fromIntegral b) > 1e-4 ||
                                                limit > 0 && (a > limit || b > limit) = 0
                                          | otherwise = a * 3 + b
    where left = (2><2) $ (\[[aX, aY], [bX, bY]] -> [aX, bX, aY, bY]) $ map (map (read . drop 2) . splitOn ", " . drop 10) [bA, bB]
          right = (2><1) $ map ((+ resAdd) . read . drop 2) $ splitOn ", " $ drop 7 prize
          result = linearSolve left right
          [aD, bD] = concat $ toLists $ fromJust result
          a = round aD
          b = round bD
getMinTokens _ _ _ = error "Invalid input."

part1 :: Solution
part1 = V . sum . map (getMinTokens 0 100) . split null

part2 :: Solution
part2 = V . sum . map (getMinTokens 10000000000000 0) . split null
