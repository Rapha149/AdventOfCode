module Main where

splitNumbers :: String -> Int -> [Int] -> [Int]
splitNumbers nStr nStrLength ns = (read x) : (read y) : changeNumbersOnce ns
    where (x, y) = splitAt (nStrLength `div` 2) nStr

changeNumbersOnce :: [Int] -> [Int]
changeNumbersOnce [] = []
changeNumbersOnce (n:ns) | n == 0 = 1 : changeNumbersOnce ns
                         | nStrLength `mod` 2 == 0 = splitNumbers nStr nStrLength ns
                         | otherwise = n * 2024 : changeNumbersOnce ns
    where nStr = show n
          nStrLength = length nStr

changeNumbers :: Int -> [Int] -> [Int]
changeNumbers 0 numbers = numbers
changeNumbers i numbers = changeNumbers (i - 1) $ changeNumbersOnce numbers

main :: IO ()
main = do
    content <- readFile "input.txt"
    let numbers = map read $ words content :: [Int]
    print $ length $ changeNumbers 25 numbers
