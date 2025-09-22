module Years.Year2023.Day03 (part1, part2) where

import Util.Util
import Data.Char

isPartSymbol :: Char -> Bool
isPartSymbol '.' = False
isPartSymbol x = not $ isDigit x

adjacentPairs :: [Vec]
adjacentPairs = filter (\(x, y) -> x /= 0 || y /= 0) [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]]

getAdjacents :: Vec -> Int -> Int -> [Vec]
getAdjacents (x, y) width height = filter (inBounds0 width height) $ map add adjacentPairs
    where add (a, b) = (x + a, y + b)

parseLines :: [[Char]] -> Bool -> Int -> Int
parseLines _ _ (-1) = 0
parseLines schem gears y = parseLine (width - 1) + parseLines schem gears (y - 1)
    where height = length schem
          width = length $ hd schem
          line = schem !! y
          parseLine :: Int -> Int
          parseLine (-1) = 0
          parseLine x | isPartSymbol $ line !! x = (if gears then getGearRatio else (`sumNumbers` [])) (getAdjacents (x, y) width height) + parseLine (x - 1)
                      | otherwise = parseLine $ x - 1
          sumNumbers :: [Vec] -> [Vec] -> Int
          sumNumbers [] _ = 0
          sumNumbers (p:ps) ex | p `elem` ex = sumNumbers ps ex
                               | otherwise = let (res, ex') = digitToNumber p
                                             in res + sumNumbers ps (ex ++ ex')
          getGearRatio :: [Vec] -> Int
          getGearRatio ps = let nums = filter (> 0) $ getPartNumbers ps []
                            in if length nums == 2 then product nums else 0
          getPartNumbers :: [Vec] -> [Vec] -> [Int]
          getPartNumbers [] _ = []
          getPartNumbers (p:ps) ex | p `elem` ex = getPartNumbers ps ex
                                   | otherwise = let (res, ex') = digitToNumber p
                                                 in res : getPartNumbers ps (ex ++ ex')
          digitToNumber :: Vec -> (Int, [Vec])
          digitToNumber (nx, ny) | not $ isDigit $ schem !! ny !! nx = (0, [])
                                 | otherwise = let start = look nx ny (-1)
                                                   end = look nx ny 1
                                               in (read $ take (end - start + 1) $ drop start $ schem !! ny, map (, ny) [start..end])
          look :: Int -> Int -> Int -> Int
          look nx ny add | nx >= 0 && nx < width && isDigit (schem !! ny !! nx) = look (nx + add) ny add
                         | otherwise = nx - add

part1 :: Solution
part1 input = V $ parseLines input False (length input - 1)

part2 :: Solution
part2 input = V $ parseLines input True (length input - 1)
