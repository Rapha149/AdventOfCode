module Main where
import System.Environment (getArgs)
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map

digits :: Map.Map String Char
digits = Map.fromList $ map (\d -> (show d, (show d) !! 0)) [0..9] ++
    [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'),
     ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

lineToNumber :: String -> Int
lineToNumber s = read [getDigit take isSuffixOf 1 1, getDigit drop isPrefixOf (-1) (length s - 1)]
    where getDigit :: (Int -> String -> String) -> (String -> String -> Bool) -> Int -> Int -> Char
          getDigit sub matches add i = let substr = sub i s
                                           matching = filter (\d -> matches d substr) $ Map.keys digits
                                       in if null matching then getDigit sub matches add $ i + add else digits Map.! (matching !! 0)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    print $ sum $ map lineToNumber $ lines content
