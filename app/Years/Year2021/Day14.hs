module Years.Year2021.Day14 (part1, part2) where

import Util.Util
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Pair = (Char, Char)

step :: Map Pair Char -> Map Pair Int -> Map Pair Int
step rules = Map.foldrWithKey stepPair Map.empty
    where stepPair :: Pair -> Int -> Map Pair Int -> Map Pair Int
          stepPair (a, b) n = case rules Map.!? (a, b) of
                                     Just c -> Map.insertWith (+) (a, c) n . Map.insertWith (+) (c, b) n
                                     Nothing -> Map.insertWith (+) (a, b) n

getResult :: Int -> [String] -> Int
getResult steps input = maximum quantities - minimum quantities
    where template = hd input
          rules = Map.fromList $ map ((\xs -> (pair $ hd xs, hd $ lst xs)) . words) $ drop 2 input
          initCounts = Map.fromListWith (+) $ zipWith (curry (, 1)) template (tl template)
          counts = foldr ($) initCounts $ replicate steps (step rules)
          quantities = Map.elems $ Map.insertWith (+) (hd template) 1 $ Map.mapKeysWith (+) snd counts

part1 :: Solution
part1 = V . getResult 10

part2 :: Solution
part2 = V . getResult 40
