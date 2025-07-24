module Main where

-- isSafe :: Maybe Int -> [Int] -> Bool -> Bool
-- isSafe lastC [] asc = True
-- isSafe Nothing (c:cs) asc = isSafe (Just c) cs asc
-- isSafe (Just lastC) (c:cs) asc = lastC /= c && asc == (lastC < c) && abs (lastC - c) <= 3 && isSafe (Just c) cs asc
--
-- isReportSafe :: [Int] -> Bool
-- isReportSafe cs = isSafe Nothing cs True || isSafe Nothing cs False

isReportSafe :: [Int] -> Bool
isReportSafe xs = isSafe xs
    where asc = xs !! 0 < xs !! 1
          isSafe (x:[]) = True
          isSafe (a:b:xs) = a /= b && asc == (a < b) && abs (a - b) <= 3 && isSafe (b:xs)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let reports = map (map read . words) $ lines content
    print $ length $ filter isReportSafe reports
