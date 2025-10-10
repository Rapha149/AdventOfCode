module Years.Year2024.Day14 (part1, part2) where

import Util.Util
import Data.List.Extra

type Robot = (Vec, Vec)

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

findChristmasTree :: [Robot] -> Int -> [Int] -> (Int, [Vec])
findChristmasTree _ 10000 _ = (0, [])
findChristmasTree robots 0 _ = findChristmasTree (map (move 101 103) robots) 1 [getAvgDistance $ map fst robots]
findChristmasTree robots i ds | i >= 100 && change >= 100 = (i, map fst robots)
                              | otherwise = findChristmasTree (map (move 101 103) robots) (i + 1) (dist:ds)
    where dist = getAvgDistance $ map fst robots
          change = minimum $ map (abs . (dist -)) ds

robotsToString :: [Vec] -> [[Vec]] -> String
robotsToString _ [] = ""
robotsToString robots ([]:ys) = '\n' : robotsToString robots ys
robotsToString robots ((p:xs):ys) = (if p `elem` robots then '#' else '.') : robotsToString robots (xs:ys)

part1 :: Solution
part1 raw = let ((width, height), input) = getExtraInts 2 (pair . map read) (101, 103) raw
                robots = map (pair . map (pair . map read . splitOn "," . drop 2) . words) input
                moved = map (fst . foldr (.) id (replicate 100 (move width height))) robots
            in V $ product $ map length $ group $ sort $ filter (/= Nothing) $ map (getQuadrant (width `div` 2) (height `div` 2)) moved

part2 :: Solution
part2 input = let robots = map (pair . map (pair . map read . splitOn "," . drop 2) . words) input
                  (n, robots') = findChristmasTree robots 0 []
              in VMsg n $ robotsToString robots' [[(x, y) | x <- [0..100]] | y <- [0..102]]
