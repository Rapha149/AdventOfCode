module Main where
import Data.List

convertToLists :: [String] -> [Int] -> [Int] -> ([Int], [Int])
convertToLists [] a b = (sort a, sort b)
convertToLists (c:cs) a b = convertToLists cs ((read (head parts)) : a) ((read (last parts)) : b)
    where parts = words c

calcDifference :: [Int] -> [Int] -> Int
calcDifference [] [] = 0
calcDifference (a:as) (b:bs) = abs (a - b) + calcDifference as bs

main :: IO ()
main = do
    content <- readFile "input.txt"
    let contentLines = lines content
        lists = convertToLists contentLines [] []
        difference = calcDifference (fst lists) (snd lists)
    print difference
