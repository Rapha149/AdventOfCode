module Main where

stringToReport :: String -> [Int]
stringToReport str = map read (words str)

isSafe :: Maybe Int -> [Int] -> Bool -> Bool
isSafe _ [] _ = True
isSafe Nothing (c:cs) asc = isSafe (Just c) cs asc
isSafe (Just lastC) (c:cs) asc = lastC /= c && asc == (lastC < c) && abs (lastC - c) <= 3 && isSafe (Just c) cs asc

isReportSafeRm :: Int -> [Int] -> Bool
isReportSafeRm (-1) cs = False
isReportSafeRm i cs = isSafe Nothing ds True || isSafe Nothing ds False || isReportSafeRm (i - 1) cs
    where ds = take i cs ++ drop (i + 1) cs

isReportSafe :: [Int] -> Bool
isReportSafe cs = isSafe Nothing cs True || isSafe Nothing cs False || isReportSafeRm (length cs) cs

countSafeReports :: [[Int]] -> Int
countSafeReports = length . filter isReportSafe

main :: IO ()
main = do
    content <- readFile "input.txt"
    let contentLines = lines content
        reports = map stringToReport contentLines
        count = countSafeReports reports
    print count
