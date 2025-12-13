module Years.Year2025.Day12 (part1) where

import Util.Util
import Data.Bits
import Data.List.Extra
import Data.Tuple.Extra
import Data.Heap (MinHeap)
import qualified Data.Heap as H
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Bitmask = Integer
type Form = [Vec]
data Shape = Shape { area :: Int, transformations :: [Form] }
data Region = Region { width :: Int, height :: Int, presents :: Map Int Int } deriving Show

transform :: Form -> [Form]
transform grid = let rotated = take 4 $ iterate rotate90 grid
                     flipped = map flipHorizontal rotated
                     normalized = map normalize (rotated ++ flipped)
                 in Set.toList $ Set.fromList normalized
    where rotate90 :: Form -> Form
          rotate90 s = [(-c, r) | (r, c) <- s]
          flipHorizontal :: Form -> Form
          flipHorizontal = map (first negate)
          normalize :: Form -> Form
          normalize s = let (minR, minC) = both minimum $ unzip s
                        in sort [(r - minR, c - minC) | (r, c) <- s]

parseInput :: [String] -> ([Shape], [Region])
parseInput input = (map parseShape $ ini parts, map (parseRegion . words) $ lst parts)
    where parts = split null input
          parseShape :: [String] -> Shape
          parseShape line = let grid = [(r, c) | (r, row) <- zip [0..] line, (c, '#') <- zip [0..] row]
                            in Shape (length grid) (transform grid)
          parseRegion :: [String] -> Region
          parseRegion [] = error "Empty list."
          parseRegion (size:counts) = let (width, height) = pair $ sort $ map read $ splitOn "x" $ ini size
                                          presents = Map.fromList $ zip [0..] $ map read counts
                                      in Region {..}

place :: Int -> Int -> Bitmask -> Int -> Int -> Form -> Maybe Bitmask
place width height bits r c form | or [r + dr >= height || c + dc >= width | (dr, dc) <- form] || bits .&. mask /= 0 = Nothing
                                 | otherwise = Just $ bits .|. mask
    where mask = foldl' setBit 0 [width * (r + dr) + (c + dc) | (dr, dc) <- form]

findArrangement :: Int -> Int -> [Shape] -> MinHeap (Int, Int, Map Int Int, Bitmask) -> Bool
findArrangement width height shapes heap | null heap = False
                                         | needed > width * height - pos - popCount (bits `shiftR` pos) = findArrangement width height shapes rest
                                         | left <= ((height - row - 3) `div` 3) * (width `div` 3) = True
                                         | otherwise = findArrangement width height shapes $ foldr H.insert (H.insert (left, pos + 1, presents, bits) rest) next
    where Just ((left, pos, presents, bits), rest) = H.view heap
          needed = sum $ Map.mapWithKey (\i count -> (shapes !! i).area * count) presents
          (row, col) = pos `divMod` width
          next = [(left - 1, pos + 1, presents', bits') | (i, count) <- Map.toList presents, count > 0, let presents' = Map.insert i (count - 1) presents,
                    Just bits' <- map (place width height bits row col) (shapes !! i).transformations]

fits :: [Shape] -> Region -> Bool
fits shapes Region{..} = findArrangement width height shapes $ H.singleton (sum presents, 0, presents, 0)

part1 :: Solution
part1 input = V $ length $ filter (fits shapes) regions
    where (shapes, regions) = parseInput input
