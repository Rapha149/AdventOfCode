module Years.Year2023.Day04 (part1, part2) where

import Util.Util
import Data.List.Split
import qualified Data.Map.Strict as Map

getCardPoints :: String -> Int
getCardPoints card | correct > 0 = 2 ^ (correct - 1)
                   | otherwise = 0
    where numbers = splitOn "|" $ splitOn ":" card !! 1
          winning = words $ hd numbers
          yours = words $ numbers !! 1
          correct = length $ filter (`elem` winning) yours

part1 :: Solution
part1 = V . sum . map getCardPoints


getCardCounts :: [String] -> Int -> Map.Map Int Int -> Map.Map Int Int
getCardCounts [] _ counts = counts
getCardCounts (card:cs) i counts | correct > 0 = getCardCounts cs (i + 1) $ foldr (\x -> Map.insertWith (+) x count) counts [i+1 .. i+correct]
                                 | otherwise = getCardCounts cs (i + 1) counts
    where numbers = splitOn "|" $ splitOn ":" card !! 1
          winning = words $ hd numbers
          yours = words $ numbers !! 1
          correct = length (filter (`elem` winning) yours)
          count = counts Map.! i

part2 :: Solution
part2 input = V $ Map.foldr (+) 0 $ getCardCounts input 1 $ Map.fromList $ map (, 1) [1..length input]
