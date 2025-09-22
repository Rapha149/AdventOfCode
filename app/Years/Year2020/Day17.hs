module Years.Year2020.Day17 (part1, part2) where

import Util.Util
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

doCycle :: Set [Int] -> Set [Int]
doCycle active = Set.union new $ Set.intersection active stay
    where neighborCounts = foldr (\xs -> Map.unionWith (+) $ Map.fromList [(zipWith (+) xs ds, 1 :: Int) | ds <- replicateM (length xs) [-1..1], any (/= 0) ds]) Map.empty active
          stay = Map.keysSet $ Map.filter (== 2) neighborCounts
          new = Map.keysSet $ Map.filter (== 3) neighborCounts

getResult :: Int -> [String] -> Int
getResult dim input = Set.size $ foldr ($) initActive $ replicate 6 doCycle
    where zeros = replicate (dim - 2) 0
          initActive = Set.fromList [r : c : zeros | (r, row) <- zip [0..] input, (c, '#') <- zip [0..] row]

part1 :: Solution
part1 = V . getResult 3

part2 :: Solution
part2 = V . getResult 4
