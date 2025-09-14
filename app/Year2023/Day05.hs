module Year2023.Day05 (part1, part2) where

import Util
import Data.List.Extra

type Category = [(Int, Int, Int)]
type Range = (Int, Int)

parseCategory :: [String] -> Category
parseCategory (_:ls) = map (parseLine . map read . words) ls
    where parseLine :: [Int] -> (Int, Int, Int)
          parseLine [d,s,l] = (d, s, s + l)
          parseLine _ = error "Not three elements."
parseCategory _ = error "Empty list"

mapNumber :: [Category] -> Int -> Int
mapNumber [] x = x
mapNumber (m:ms) x = mapNumber ms $ mapNumber' m
    where mapNumber' :: Category -> Int
          mapNumber' [] = x
          mapNumber' ((d,a,b):xs) | x >= a && x < b = x - a + d
                                  | otherwise = mapNumber' xs

parseSeedRanges :: [Int] -> [Range]
parseSeedRanges [] = []
parseSeedRanges (a:b:xs) = (a, a + b) : parseSeedRanges xs
parseSeedRanges (_:_) = error "Uneven number of seeds."

intersection :: Int -> Int -> Int -> Int -> (Bool, Int, Int)
intersection a b c d | e < f = (True, e, f)
                     | otherwise = (False, 0, 0)
    where e = max a c
          f = min b d

mapSeedRanges :: [Range] -> Category -> [Range]
mapSeedRanges [] _ = []
mapSeedRanges ((x,y):xs) category = checkCategory category
    where checkCategory :: Category -> [Range]
          checkCategory [] = (x,y) : mapSeedRanges xs category
          checkCategory ((d,a,b):ms) = let (exists, ix, iy) = intersection x y a b
                                       in if exists then handleCategory ix iy (d - a) else checkCategory ms
          handleCategory :: Int -> Int -> Int -> [Range]
          handleCategory ix iy d | x < ix && iy < y = (ix + d, iy + d) : mapSeedRanges ((x,ix):(iy,y):xs) category
                                 | x < ix = (ix + d, y + d) : mapSeedRanges ((x,ix):xs) category
                                 | iy < y = (x + d, iy + d) : mapSeedRanges ((iy,y):xs) category
                                 | otherwise = (x + d, y + d) : mapSeedRanges xs category

part1 :: Solution
part1 input = let parts = split null input
                  seeds = map read $ words $ drop 7 $ hd $ hd parts :: [Int]
                  categories = map parseCategory $ drop 1 parts
              in V $ minimum $ map (mapNumber categories) seeds

part2 :: Solution
part2 input = let parts = split null input
                  seedRanges = parseSeedRanges $ map read $ words $ drop 7 $ hd $ hd parts
                  categories = map parseCategory $ drop 1 parts
              in V $ minimum $ map fst $ foldl mapSeedRanges seedRanges categories
