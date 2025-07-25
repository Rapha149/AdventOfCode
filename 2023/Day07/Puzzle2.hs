module Main where
import System.Environment (getArgs)
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map

cards = "J23456789TQKA"

getHandType :: String -> Int
getHandType hand | jokers == 5 = 6
                 | otherwise = maximum $ map (\c -> toType $ sort $ Map.elems $ Map.adjust (+ jokers) c $ amounts) $ Map.keys amounts
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

compareHands :: (String, Int, Int) -> (String, Int, Int) -> Ordering
compareHands (h1, t1, _ ) (h2, t2, _) | t1 /= t2 = compare t1 t2
                                      | otherwise = compareCards h1 h2
    where compareCards :: String -> String -> Ordering
          compareCards [] [] = EQ
          compareCards (c1:cs1) (c2:cs2) | c1 /= c2 = compare (fromJust $ elemIndex c1 cards) (fromJust $ elemIndex c2 cards)
                                         | otherwise = compareCards cs1 cs2

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    let hands = map (\[hand, bid] -> (hand, getHandType hand, read bid :: Int)) $ map words $ lines content
    print $ sum $ map (uncurry (*)) $ zip (map (\(_, _, bid) -> bid) $ sortBy compareHands hands) [1..]
