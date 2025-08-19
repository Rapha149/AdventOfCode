module Year2023.Day07 (part1, part2) where

import Util
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map

cards :: Bool -> [Char]
cards False = "23456789TJQKA"
cards True = "J23456789TQKA"

getHandType :: Bool -> String -> Int
getHandType b hand | not b = toType $ sort $ Map.elems $ Map.fromListWith (+) $ map (, 1) hand
                   | jokers == 5 = 6
                   | otherwise = maximum $ map (\c -> toType $ sort $ Map.elems $ Map.adjust (+ jokers) c amounts) $ Map.keys amounts
    where amounts = Map.fromListWith (+) $ map (, 1) $ filter (/= 'J') hand
          jokers = length $ filter (== 'J') hand
          toType :: [Int] -> Int
          toType [5] = 6
          toType [1, 4] = 5
          toType [2, 3] = 4
          toType [1, 1, 3] = 3
          toType [1, 2, 2] = 2
          toType [1, 1, 1, 2] = 1
          toType [1, 1, 1, 1, 1] = 0
          toType _ = error "Invalid type."

compareHands :: Bool -> (String, Int, Int) -> (String, Int, Int) -> Ordering
compareHands b (h1, t1, _ ) (h2, t2, _) | t1 /= t2 = compare t1 t2
                                        | otherwise = compareCards h1 h2
    where compareCards :: String -> String -> Ordering
          compareCards [] [] = EQ
          compareCards (c1:cs1) (c2:cs2) | c1 /= c2 = compare (fromJust $ elemIndex c1 $ cards b) (fromJust $ elemIndex c2 $ cards b)
                                         | otherwise = compareCards cs1 cs2
          compareCards _ _ = error "Invalid number of cards."

getWinnings :: Bool -> [String] -> Int
getWinnings b input = let hands = map ((\(hand, bid) -> (hand, getHandType b hand, read bid :: Int)) . tuple . words) input
                      in sum $ zipWith (*) (map (\(_, _, bid) -> bid) $ sortBy (compareHands b) hands) [1..]

part1 :: Solution
part1 = V . getWinnings False

part2 :: Solution
part2 = V . getWinnings True
