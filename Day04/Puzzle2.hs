module Main where
import Data.List
import Text.Regex.TDFA

count :: [String] -> Int -> Int -> Int
count matrix row col | row + 2 >= length matrix = 0
                         | col + 2 >= length (matrix !! 0) = count matrix (row + 1) 0
                         | otherwise = fromEnum (matrix !! row !! col == 'M' &&  matrix !! row !! (col + 2) == 'M' &&
                                                matrix !! (row + 1) !! (col + 1) == 'A' &&
                                                matrix !! (row + 2) !! col == 'S' && matrix !! (row + 2) !! (col + 2) == 'S')
                                       + count matrix row (col + 1)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
        columns = transpose rows
    print $ count rows 0 0 + count (reverse rows) 0 0 + count columns 0 0 + count (reverse columns) 0 0
