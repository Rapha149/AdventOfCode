module Main where
import System.Environment (getArgs)

numberOfWaysToWin :: (Int, Int) -> Int
numberOfWaysToWin (time, distance) = length $ filter (\i -> (time - i) * i > distance) [1..time - 1]

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    let races = (\[a,b] -> zip a b) $ map (map read . drop 1 . words) $ lines content :: [(Int, Int)]
    print $ product $ map numberOfWaysToWin races
