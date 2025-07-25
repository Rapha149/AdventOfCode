module Main where
import System.Environment (getArgs)
import Data.List

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    let [time, distance] = map (read . intercalate [] . drop 1 . words) $ lines content :: [Int]
    print $ length $ filter (\i -> (time - i) * i > distance) [1..time - 1]
