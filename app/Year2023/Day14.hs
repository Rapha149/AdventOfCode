module Year2023.Day14 (part1, part2) where

import Util
import Data.Ord
import Data.List.Extra
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Rocks = Map Vec Bool

parse :: [String] -> (Int, Int, Rocks)
parse input = (height, width, Map.fromList $ map (second (== 'O')) $ filter ((/= '.') . snd) [((r,c), input !! r !! c) | r <- [0..height-1], c <- [0..width-1]])
    where height = length input
          width = length $ hd input

roll :: Int -> Int -> (Vec -> Vec) -> ([Vec] -> [Vec]) -> Rocks -> Rocks
roll height width add sortBalls rocks = foldl rollEach cubes $ sortBalls $ Map.keys balls
    where (balls, cubes) = Map.partition id rocks
          rollEach :: Rocks -> Vec -> Rocks
          rollEach stoneMap pos = let next = add pos
                                  in if inBounds0 height width next && next `Map.notMember` stoneMap
                                        then rollEach stoneMap next
                                        else Map.insert pos True stoneMap

spin :: Int -> Int -> Rocks -> Rocks
spin h w = roll h w (second (+1)) (sortOn (Down . snd)) .
           roll h w (first (+1)) reverse .
           roll h w (second (subtract 1)) (sortOn snd) .
           roll h w (first (subtract 1)) id

detectRepeats :: Set Rocks -> Int -> Int -> Rocks -> Bool -> Vec
detectRepeats previous height width rocks seen | next `Set.member` previous = if seen then (1,1) else detectRepeats (Set.singleton next) height width next True
                                               | otherwise = (if seen then second else first) (+1) $ detectRepeats (Set.insert next previous) height width next seen
    where next = spin height width rocks

totalLoad :: Int -> Rocks -> Int
totalLoad height = sum . map ((height -) . fst) . Map.keys . Map.filter id

part1 :: Solution
part1 input = let (height, width, rocks) = parse input
              in V $ totalLoad height $ roll height width (first (subtract 1)) id rocks

part2 :: Solution
part2 input = let (height, width, rocks) = parse input
                  (s, r) = detectRepeats (Set.singleton rocks) height width rocks False
              in V $ totalLoad height $ foldr ($) rocks (replicate (s + (1000000000 - s) `mod` r) (spin height width))
