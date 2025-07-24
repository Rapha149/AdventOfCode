module Main where
import System.Environment (getArgs)
import Data.List.Split

getCardPoints :: String -> Int
getCardPoints card | correct > 0 = 2 ^ (correct - 1)
                   | otherwise = 0
    where numbers = splitOn "|" $ (splitOn ":" card) !! 1
          winning = words $ numbers !! 0
          yours = words $ numbers !! 1
          correct = length $ filter (`elem` winning) yours

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    print $ sum $ map getCardPoints $ lines content
