module Main where

isReportSafe :: [Int] -> Bool
isReportSafe xs = any (\ys -> isSafe ys $ ys !! 0 < ys !! 1) $ map (\i -> take i xs ++ drop (i + 1) xs) [0..length xs]
    where isSafe (x:[]) _ = True
          isSafe (a:b:xs) asc = a /= b && asc == (a < b) && abs (a - b) <= 3 && isSafe (b:xs) asc

main :: IO ()
main = do
    content <- readFile "input.txt"
    let reports = map (map read . words) $ lines content
    print $ length $ filter isReportSafe reports
