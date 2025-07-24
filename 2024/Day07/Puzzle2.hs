module Main where
import Data.List.HT
import Data.List.Split

parseLines :: [String] -> [(Int, [Int])]
parseLines [] = []
parseLines (l:ls) = (read $ head split, numbers) : parseLines ls
    where split = splitOn ":" l
          numbers = map read $ words $ last split

getOperators :: Int -> [Int]
getOperators n | n <= 2 = [n]
               | otherwise = n `mod` 3 : (getOperators $ div n 3)

calcPossibility :: [Int] -> [Int] -> Int
calcPossibility [n] [] = n
calcPossibility (n:ns) (o:os) | o == 0 = calcPossibility ns os + n
                              | o == 1 = calcPossibility ns os * n
                              | o == 2 = read $ (show $ calcPossibility ns os) ++ show n

isPossible :: Int -> [Int] -> [Int] -> Bool
isPossible _ _ [] = False
isPossible result numbers (p:ps) = result == (calcPossibility numbers ops) || isPossible result numbers ps
    where ops = padRight 0 (length numbers - 1) $ getOperators p

sumPossibleEquationResults :: [(Int, [Int])] -> Int
sumPossibleEquationResults [] = 0
sumPossibleEquationResults (e:es) | isPossible result (reverse numbers) [0 .. 3 ^ (length numbers - 1) - 1] = result + sumPossibleEquationResults es
                                  | otherwise = sumPossibleEquationResults es
    where (result, numbers) = e

main :: IO ()
main = do
    content <- readFile "input.txt"
    let contentLines = lines content
        equations = parseLines contentLines
    print $ sumPossibleEquationResults equations
