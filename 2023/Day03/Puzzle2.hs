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
          parseLine x | isPartSymbol $ line !! x = getGearRatio (getAdjacents (x, y) width height) + parseLine (x - 1)
                      | otherwise = parseLine $ x - 1
          getGearRatio :: [(Int, Int)] -> Int
          getGearRatio ps = let nums = filter (>= 0) $ getPartNumbers ps []
                            in if length nums == 2 then product nums else 0
          getPartNumbers :: [(Int, Int)] -> [(Int, Int)] -> [Int]
          getPartNumbers [] _ = []
          getPartNumbers (p:ps) ex | elem p ex = getPartNumbers ps ex
                                   | otherwise = let (res, ex') = digitToNumber p
                                                 in res : getPartNumbers ps (ex ++ ex')
          digitToNumber :: (Int, Int) -> (Int, [(Int, Int)])
          digitToNumber (x, y) | not $ isDigit $ schem !! y !! x = (-1, [])
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
