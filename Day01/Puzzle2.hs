module Main where
import Data.List

convertToLists :: [String] -> [Int] -> [Int] -> ([Int], [Int])
convertToLists [] a b = (sort a, sort b)
convertToLists (c:cs) a b = convertToLists cs ((read (head parts)) : a) ((read (last parts)) : b)
    where parts = words c

calcSimilarity :: [Int] -> [Int] -> Int
-- calcSimilarity [] [] = 0
calcSimilarity (a:[]) b = a * ((length . filter (== a)) b)
calcSimilarity (a:as) b = a * ((length . filter (== a)) b) + calcSimilarity as b

main :: IO ()
main = do
    content <- readFile "input.txt"
    let contentLines = lines content
        lists = convertToLists contentLines [] []
        similarity = calcSimilarity (fst lists) (snd lists)
    print similarity
