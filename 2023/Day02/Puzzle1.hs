module Main where
import System.Environment (getArgs)
import Data.List.Split
import qualified Data.Map.Strict as Map

config = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]

getPossibleGameNumbers :: [String] -> [Int]
getPossibleGameNumbers [] = []
getPossibleGameNumbers (g:gs) = let [a, b] = splitOn ":" g
                                    num = read $ drop 5 a
                                in if isGamePossible b then num : getPossibleGameNumbers gs else getPossibleGameNumbers gs

isGamePossible :: String -> Bool
isGamePossible = all (\[a, b] -> read a <= config Map.! b) . map words . splitOneOf ",;"

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    print $ sum $ getPossibleGameNumbers $ lines content
