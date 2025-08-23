module Year2024.Day14 (part1, part2) where

import Util
import Data.List
import Data.List.Extra

type Robot = (Vec, Vec)
type State = (Int, [Vec])

move :: Int -> Int -> Robot -> Robot
move width height ((x, y), (dx, dy)) = (((x + dx) `mod` width, (y + dy) `mod` height), (dx, dy))

getQuadrant :: Int -> Int -> Vec -> Maybe Int
getQuadrant mx my (x, y) | x == mx || y == my = Nothing
                         | otherwise = Just $ fromEnum (x > mx) + fromEnum (y > my) * 2

average :: [Int] -> Int
average xs = sum xs `div` length xs

getAvgDistance :: [Vec] -> Int
getAvgDistance robots = let avgX = average $ map fst robots
                            avgY = average $ map snd robots
                            square x = x * x
                        in average $ map (\(x, y) -> square (x - avgX) + square (y - avgY)) robots

getNStates :: Int -> Int -> Int -> Int -> [Robot] -> [State]
getNStates width height n i robots | i >= n = []
                                        | otherwise = (i, map fst robots) : getNStates width height n (i + 1) (map (move width height) robots)

stateToString :: State -> String
stateToString (i, robots) = show i <> ":\n" <> grid 0 0
    where grid :: Int -> Int -> String
          grid _ 103 = ""
          grid 101 y = '\n' : grid 0 (y + 1)
          grid x y = (if (x, y) `elem` robots then '#' else '.') : grid (x + 1) y

part1 :: Solution
part1 rawInput = let ((width, height), input) = getExtraInts 2 (tuple . map read) (101, 103) rawInput
                     robots = map (tuple . map (tuple . map read . splitOn "," . drop 2) . words) input
                     moved = map (fst . foldr (.) id (replicate 100 (move width height))) robots
                 in V $ product $ map length $ group $ sort $ filter (/= Nothing) $ map (getQuadrant (width `div` 2) (height `div` 2)) moved

part2 :: Solution
part2 input = let limit = read $ hd input :: Int
                  robots = map (tuple . map (tuple . map read . splitOn "," . drop 2) . words) $ drop 1 input
              in Msg $ stateToString $ minimumOn (getAvgDistance . snd) $ getNStates 101 103 limit 0 robots
