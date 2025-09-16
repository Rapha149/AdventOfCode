module Year2020.Day24 (part1, part2) where

import Util
import Data.Bifunctor
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

identifyTile :: String -> Vec
identifyTile "" = (0, 0)
identifyTile ('e':xs) = first (+ 2) $ identifyTile xs
identifyTile ('s':'e':xs) = bimap (+ 1) (subtract 1) $ identifyTile xs
identifyTile ('s':'w':xs) = bimap (subtract 1) (subtract 1) $ identifyTile xs
identifyTile ('w':xs) = first (subtract 2) $ identifyTile xs
identifyTile ('n':'w':xs) = bimap (subtract 1) (+ 1) $ identifyTile xs
identifyTile ('n':'e':xs) = bimap (+ 1) (+ 1) $ identifyTile xs
identifyTile _ = error "Invalid step."

getFlipped :: [String] -> Set Vec
getFlipped = Map.keysSet . Map.filter id . Map.fromListWith (/=) . map ((, True) . identifyTile)

part1 :: Solution
part1 = V . Set.size . getFlipped


flipTiles :: Set Vec -> Set Vec
flipTiles black = Set.union new $ Set.intersection black stay
    where neighborCounts = foldr (\tile -> Map.unionWith (+) $ Map.fromList $ map ((, 1 :: Int) . onBoth (+) tile) [(2, 0), (1, -1), (-1, -1), (-2, 0), (-1, 1), (1, 1)]) Map.empty black
          stay = Map.keysSet $ Map.filter (`elem` [1, 2]) neighborCounts
          new = Map.keysSet $ Map.filter (== 2) neighborCounts

part2 :: Solution
part2 = V . Set.size . foldr (.) id (replicate 100 flipTiles) . getFlipped
