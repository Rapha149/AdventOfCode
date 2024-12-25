module Main where

stringToReport :: String -> [Int]
stringToReport str = map read (words str)

isSafe :: Maybe Int -> [Int] -> Bool -> Bool
isSafe lastC [] asc = True
isSafe Nothing (c:cs) asc = isSafe (Just c) cs asc
isSafe (Just lastC) (c:cs) asc = lastC /= c && asc == (lastC < c) && abs (lastC - c) <= 3 && isSafe (Just c) cs asc

isReportSafe :: [Int] -> Bool
isReportSafe cs = isSafe Nothing cs True || isSafe Nothing cs False

countSafeReports :: [[Int]] -> Int
countSafeReports = length . filter isReportSafe

main :: IO ()
main = do
    content <- readFile "input.txt"
    let contentLines = lines content
        reports = map stringToReport contentLines
        count = countSafeReports reports
    print count
