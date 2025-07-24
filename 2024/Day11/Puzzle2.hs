module Main where
import qualified Data.Map as Map

changeNumbersOnce :: [(Int, Int)] -> Map.Map Int Int -> Map.Map Int Int
changeNumbersOnce [] new = new
changeNumbersOnce (n:ns) new | k == 0 = changeNumbersOnce ns $ Map.insertWith (+) 1 v new
                             | kStrLength `mod` 2 == 0 = let (x, y) = splitAt (kStrLength `div` 2) kStr
                                                         in changeNumbersOnce ns $ Map.insertWith (+) (read x) v $ Map.insertWith (+) (read y) v new
                             | otherwise = changeNumbersOnce ns $ Map.insertWith (+) (k * 2024) v new
    where (k, v) = n
          kStr = show k
          kStrLength = length kStr

changeNumbers :: Int -> Map.Map Int Int -> Map.Map Int Int
changeNumbers 0 numbers = numbers
changeNumbers i numbers = changeNumbers (i - 1) $ changeNumbersOnce (Map.assocs numbers) Map.empty

main :: IO ()
main = do
    content <- readFile "input.txt"
    let numbers = map read $ words content :: [Int]
        numberMap = Map.fromListWith (+) $ map (\x -> (x, 1)) numbers
    print $ sum $ Map.elems $ changeNumbers 75 numberMap
