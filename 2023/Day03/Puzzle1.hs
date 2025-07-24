module Main where
import System.Environment (getArgs)
import Data.Char

isPartSymbol :: Char -> Bool
isPartSymbol '.' = False
isPartSymbol x = not $ isDigit x

adjacentPairs = filter (\(x, y) -> x /= 0 || y /= 0) [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]]

getAdjacents :: (Int, Int) -> Int -> Int -> [(Int, Int)]
getAdjacents (x, y) width height = filter withinBounds $ map add adjacentPairs
    where add (a, b) = (x + a, y + b)
          withinBounds (x, y) = and [x >= 0, y >= 0, x < width, y < height]

parseLines :: [[Char]] -> Int -> Int
parseLines _ (-1) = 0
parseLines schem y = parseLine (width - 1) + parseLines schem (y - 1)
    where height = length schem
          width = length $ schem !! 0
          line = schem !! y
          parseLine :: Int -> Int
          parseLine (-1) = 0
          parseLine x | isPartSymbol $ line !! x = sumNumbers (getAdjacents (x, y) width height) [] + parseLine (x - 1)
                      | otherwise = parseLine $ x - 1
          sumNumbers :: [(Int, Int)] -> [(Int, Int)] -> Int
          sumNumbers [] _ = 0
          sumNumbers (p:ps) ex | elem p ex = sumNumbers ps ex
                               | otherwise = let (res, ex') = digitToNumber p
                                             in res + sumNumbers ps (ex ++ ex')
          digitToNumber :: (Int, Int) -> (Int, [(Int, Int)])
          digitToNumber (x, y) | not $ isDigit $ schem !! y !! x = (0, [])
                               | otherwise = let start = look x y (-1)
                                                 end = look x y 1
                                             in (read $ take (end - start + 1) $ drop start $ schem !! y, map (, y) [start..end])
          look :: Int -> Int -> Int -> Int
          look x y add | x >= 0 && x < width && isDigit (schem !! y !! x) = look (x + add) y add
                       | otherwise = x - add

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    let contentLines = lines content
    print $ parseLines contentLines (length contentLines - 1)
