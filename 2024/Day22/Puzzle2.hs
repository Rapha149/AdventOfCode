module Main where
import Data.Maybe
import Data.Bits
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map

getLastDigit :: Int -> Int
getLastDigit n = digitToInt $ nStr !! (length nStr - 1)
    where nStr = show n

generatePrices :: Int -> Int -> [Int]
generatePrices 0 n = [getLastDigit n]
generatePrices i n = getLastDigit n : generatePrices (i - 1) n'''
    where n' = (n * 64 `xor` n) `mod` 16777216
          n'' = (n' `div` 32 `xor` n') `mod` 16777216
          n''' = (n'' * 2048 `xor` n'') `mod` 16777216

type Sequence = (Int, Int, Int, Int)

toSequenceOfChanges :: [Int] -> Map.Map Sequence Int
toSequenceOfChanges (a:b:c:d:e:xs) = Map.insert (b - a, c - b, d - c, e - d) e $ toSequenceOfChanges (b:c:d:e:xs)
toSequenceOfChanges _ = Map.empty

getBananaCount :: [Map.Map Sequence Int] -> Sequence -> Int
getBananaCount [] _ = 0
getBananaCount (m:ms) s = (if count == Nothing then 0 else fromJust count) + getBananaCount ms s
    where count = Map.lookup s m

main :: IO ()
main = do
    content <- readFile "input.txt"
    let secrets = map read $ lines content :: [Int]
        sequenceMaps = map (toSequenceOfChanges . generatePrices 2000) secrets
        sequences = Set.toList $ Set.fromList $ foldr (\a b -> Map.keys a ++ b) [] sequenceMaps
    print $ maximum $ map (getBananaCount sequenceMaps) sequences
