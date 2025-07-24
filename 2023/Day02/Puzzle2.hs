module Main where
import System.Environment (getArgs)
import Data.List.Split
import qualified Data.Map.Strict as Map

getGamePower :: String -> Int
getGamePower = Map.foldr (*) 1 . Map.fromListWith max . map (\[a, b] -> (b, read a)) . map words . splitOneOf ",;"

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    print $ sum $ map getGamePower $ map ((!! 1) . splitOn ":") $ lines content
