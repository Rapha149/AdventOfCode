module Main where
import Data.List.HT
import Data.List.Split

parseLines :: [String] -> [(Int, [Int])]
parseLines [] = []
parseLines (l:ls) = (read $ head split, numbers) : parseLines ls
    where split = splitOn ":" l
          numbers = map read $ words $ last split

getOperators :: Int -> [Bool]
getOperators 0 = []
getOperators 1 = [True]
getOperators n =  (n `mod` 2 == 1) : (getOperators $ div n 2)

calcPossibility :: [Int] -> [Bool] -> Int
calcPossibility (n:[]) [] = n
calcPossibility (n:ns) (o:os) | o = calcPossibility ns os + n
                              | otherwise = calcPossibility ns os * n

isPossible :: Int -> [Int] -> [Int] -> Bool
isPossible _ _ [] = False
isPossible result numbers (p:ps) = result == (calcPossibility numbers $ padRight False (length numbers - 1) $ getOperators p) || isPossible result numbers ps

sumPossibleEquationResults :: [(Int, [Int])] -> Int
sumPossibleEquationResults [] = 0
sumPossibleEquationResults (e:es) | isPossible result (reverse numbers) [0 .. 2 ^ (length numbers - 1) - 1] = result + sumPossibleEquationResults es
                                  | otherwise = sumPossibleEquationResults es
    where (result, numbers) = e

main :: IO ()
main = do
    content <- readFile "input.txt"
    let contentLines = lines content
        equations = parseLines contentLines
    print $ sumPossibleEquationResults equations
