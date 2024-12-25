module Main where
import Data.Char
import qualified Data.Map as Map

parseContentLine :: String -> Int -> Int -> Map.Map (Int, Int) Int -> [(Int, Int)] -> (Map.Map (Int, Int) Int, [(Int, Int)])
parseContentLine [] _ _ coords starts = (coords, starts)
parseContentLine (c:cs) x y coords starts = parseContentLine cs (x + 1) y (Map.insert (x, y) (digitToInt c) coords) $ if c == '0' then (x, y) : starts else starts

parseContent :: [String] -> Int -> Map.Map (Int, Int) Int -> [(Int, Int)] -> (Map.Map (Int, Int) Int, [(Int, Int)])
parseContent [] _ coords starts = (coords, starts)
parseContent (l:ls) y coords starts = parseContent ls (y + 1) newCoords newStarts
    where (newCoords, newStarts) = parseContentLine l 0 y coords starts

getNextPoint :: (Int, Int) -> Int -> (Int, Int)
getNextPoint point dir | dir == 0 = (x, y - 1)
                       | dir == 1 = (x + 1, y)
                       | dir == 2 = (x, y + 1)
                       | dir == 3 = (x - 1, y)
                       | otherwise = point
    where (x, y) = point

followTrail :: Map.Map (Int, Int) Int -> (Int, Int) -> Int -> Int -> Int
followTrail coords point height dir | dir > 3 = 0
                                    | nextHeightMaybe /= (Just nextHeight) = followTrail coords point height nextDir
                                    | otherwise = followTrail coords point height nextDir + if nextHeight == 9 then 1 else getTrailRating coords nextPoint nextHeight
    where nextPoint = getNextPoint point dir
          (x, y) = nextPoint
          nextHeightMaybe = Map.lookup nextPoint coords
          nextHeight = height + 1
          nextDir = dir + 1

getTrailRating :: Map.Map (Int, Int) Int -> (Int, Int) -> Int -> Int
getTrailRating coords point height = followTrail coords point height 0

sumTrailRatings :: Map.Map (Int, Int) Int -> [(Int, Int)] -> Int
sumTrailRatings _ [] = 0
sumTrailRatings coords (p:ps) = getTrailRating coords p 0 + sumTrailRatings coords ps

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (coords, starts) = parseContent (lines content) 0 Map.empty []
    print $ sumTrailRatings coords starts
