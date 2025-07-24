module Main where
import Data.List
import Text.Regex.TDFA

rowsToDiagonal :: [String] -> [String]
rowsToDiagonal matrix =
    [[matrix !! (i + r) !! i | i <- [0..rowMax-r]] | r <- [0..rowMax]] ++
    [[matrix !! i !! (i + c) | i <- [0..rowMax], i + c <= colMax] | c <- [1..colMax]] ++
    [[matrix !! (i + r) !! (colMax - i) | i <- [0..rowMax-r]] | r <- [0..rowMax]] ++
    [[matrix !! i !! (c - i) | i <- [0..rowMax], c - i >= 0] | c <- [0..colMax-1]]
    where rowMax = (length matrix - 1)
          colMax = (length (matrix !! 0) - 1)

countMatches :: [String] -> Int
countMatches [] = 0
countMatches (c:cs) = (c =~ "XMAS" :: Int) + (c =~ "SAMX" :: Int) + countMatches cs

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
        columns = transpose rows
        diagonals = rowsToDiagonal rows
    print $ countMatches (rows ++ columns ++ diagonals)
