module Main where
import Text.Regex.TDFA

parse :: String -> Int
parse str | length groups == 2 = (read (head groups)) * (read (last groups)) + parse next
          | otherwise = 0
    where (_, _, next, groups) = str =~ "mul\\(([0-9]+),([0-9]+)\\)" :: (String, String, String, [String])

main :: IO ()
main = do
    content <- readFile "input.txt"
    print (parse content)
