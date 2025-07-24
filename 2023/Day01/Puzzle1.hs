module Main where
import System.Environment (getArgs)
import Data.Char

lineToNumber :: String -> Int
lineToNumber s = read [firstDigit s, firstDigit $ reverse s]
    where firstDigit :: String -> Char
          firstDigit (d:cs) | isDigit d = d
                            | otherwise = firstDigit cs

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    print $ sum $ map lineToNumber $ lines content
