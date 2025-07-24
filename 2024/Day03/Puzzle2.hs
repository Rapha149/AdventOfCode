module Main where
import Text.Regex.TDFA

parse :: String -> Bool -> Int
parse str enabled | length match == 0 = 0
                  | match == "do()" = parse next True
                  | match == "don't()" = parse next False
                  | not enabled = parse next False
                  | length groups == 2 = (read (head groups)) * (read (last groups)) + parse next True
                  | otherwise = 0
    where (_, match, next, groups) = str =~ "do\\(\\)|don't\\(\\)|mul\\(([0-9]+),([0-9]+)\\)" :: (String, String, String, [String])

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ parse content True
