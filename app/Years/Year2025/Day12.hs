module Years.Year2025.Day12 (part1) where

import Util.Util
import Data.Bits
import Data.Word
import Data.List.Extra

data Region = Region { sizeX :: Int, sizeY :: Int, presents :: [Int] }

parseInput :: [String] -> ([Word16], [Region])
parseInput input = (presents, map (parseRegion . words) $ lst parts)
    where parts = split null input
          presents = map (\ls -> foldl setBit 0 [i | (i, '#') <- zip [0..] $ concat $ tl ls]) $ ini parts
          parseRegion :: [String] -> Region
          parseRegion [] = error "Empty list."
          parseRegion (size:counts) = let (sizeX, sizeY) = pair $ map read $ splitOn "x" $ ini size
                                      in Region sizeX sizeY $ map read counts

fits :: [Word16] -> Region -> Bool
fits presents region | size < minNeeded = False
                     | size >= maxNeeded = True
                     | otherwise = error "TODO: difficult"
    where size = region.sizeX * region.sizeY
          minNeeded = sum $ zipWith (\i n -> popCount (presents !! i) * n) [0..] region.presents
          maxNeeded = sum region.presents * 9

part1 :: Solution
part1 input = V $ length $ filter (fits presents) regions
    where (presents, regions) = parseInput input
