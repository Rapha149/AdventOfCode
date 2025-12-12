module Years.Year2025.Day12 (part1) where

import Util.Util
import Data.Bits
import Data.List.Extra
import Data.Tuple.Extra
import qualified Data.Set as Set

type Shape = [Vec]
data Region = Region { width :: Int, height :: Int, presents :: [Int] }

parseInput :: [String] -> ([Shape], [Region])
parseInput input = (shapes, map (parseRegion . words) $ lst parts)
    where parts = split null input
          shapes = map (\rs -> [(r, c) | (r, row) <- zip [0..] rs, (c, '#') <- zip [0..] row]) $ ini parts
          parseRegion :: [String] -> Region
          parseRegion [] = error "Empty list."
          parseRegion (size:counts) = let (width, height) = pair $ map read $ splitOn "x" $ ini size
                                          presents = map read counts
                                      in Region {..}

toBitPos :: Int -> Int -> Int -> Int
toBitPos width r c = width * r + c

shapeToBits :: Int -> Shape -> Int -> Int -> Integer
shapeToBits width shape r c = foldl' setBit 0 [toBitPos width (r + dr) (c + dc) | (dr, dc) <- shape]

isValid :: Int -> Int -> Integer -> Shape -> Int -> Int -> Bool
isValid width height bits shape r c = and [nr >= 0 && nr < height && nc >= 0 && nc < width &&
    not (testBit bits $ toBitPos width nr nc) | (dr, dc) <- shape, let (nr, nc) = (r + dr, c + dc)]

transform :: Shape -> [Shape]
transform shape = let rotated = take 4 $ iterate rotate90 shape
                      flipped = map flipHorizontal rotated
                      normalized = map normalize (rotated ++ flipped)
                  in Set.toList $ Set.fromList normalized
    where rotate90 :: Shape -> Shape
          rotate90 s = [(-c, r) | (r, c) <- s]
          flipHorizontal :: Shape -> Shape
          flipHorizontal = map (first negate)
          normalize :: Shape -> Shape
          normalize s = let (minR, minC) = both minimum $ unzip s
                        in sort [(r - minR, c - minC) | (r, c) <- s]

tryPlace :: Int -> Int -> [[Shape]] -> [(Int, Int)] -> Integer -> Bool
tryPlace _ _ _ [] _ = True
tryPlace width height shapes ((_, 0):xs) bits = tryPlace width height shapes xs bits
tryPlace width height shapes ((idx, count):xs) bits = or [isValid width height bits shape r c && place shape r c |
                                                            shape <- shapes !! idx, r <- [0..height - 1], c <- [0..width - 1]]
    where place :: Shape -> Int -> Int -> Bool
          place shape r c = let bits' = bits .|. shapeToBits width shape r c
                            in tryPlace width height shapes ((idx, count - 1):xs) bits'

fits :: [Shape] -> Region -> Bool
fits shapes Region{..} | size < minNeeded = False
                       | size >= maxNeeded = True
                       | otherwise = tryPlace width height (map transform shapes) (zip [0..] presents) 0
    where size = width * height
          minNeeded = sum $ zipWith (\i n -> length (shapes !! i) * n) [0..] presents
          maxNeeded = sum presents * 9

part1 :: Solution
part1 input = V $ length $ filter (fits shapes) regions
    where (shapes, regions) = parseInput input
