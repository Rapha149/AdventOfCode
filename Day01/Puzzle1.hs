module Main where
import Data.List

convertToLists :: [String] -> [Int] -> [Int] -> ([Int], [Int])
convertToLists [] a b = (sort a, sort b)
convertToLists (c:cs) a b = convertToLists cs (p1:a) (p2:b)
    where [p1, p2] = map read $ words c

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (list1, list2) = convertToLists (lines content) [] []
    print $ sum $ map (\i -> abs (list1 !! i - list2 !! i)) [0..length list1-1]
