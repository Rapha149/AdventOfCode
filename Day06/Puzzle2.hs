module Main where
import Data.Maybe
import Data.List
import Debug.Trace
import qualified Data.Set as Set

getStartPoint :: [String] -> Int -> (Int, Int)
getStartPoint [] _ = error "Did not find start position"
getStartPoint (r:rs) y | x == Nothing = getStartPoint rs (y + 1)
                       | otherwise    = (fromJust x, y)
    where x = elemIndex '^' r

getNextPoint :: (Int, Int) -> Int -> (Int, Int)
getNextPoint point dir | dir == 0 = (x, y - 1)
                       | dir == 1 = (x + 1, y)
                       | dir == 2 = (x, y + 1)
                       | dir == 3 = (x - 1, y)
    where (x, y) = point

getTurnDirection, getOppositeDirection :: Int -> Int
getTurnDirection dir = (dir + 1) `mod` 4
getOppositeDirection dir = (dir + 2) `mod` 4

getPoints :: [String] -> Int -> Int -> (Int, Int) -> Int -> Set.Set (Int, Int) -> [(Int, Int)]
getPoints rows maxX maxY point dir visited | x < 0 || x > maxX || y < 0 || y > maxY = Set.toList visited
                                           | rows !! y !! x == '#' = getPoints rows maxX maxY (getNextPoint point $ getOppositeDirection dir) (getTurnDirection dir) visited
                                           | otherwise = getPoints rows maxX maxY (getNextPoint point dir) dir (Set.insert point visited)
    where (x, y) = point

isFinite :: [String] -> Int -> Int -> (Int, Int) -> Int -> Set.Set ((Int, Int), Int) -> Bool
isFinite rows maxX maxY point dir visited | x < 0 || x > maxX || y < 0 || y > maxY = True
                                          | rows !! y !! x == '#' = isFinite rows maxX maxY (getNextPoint point $ getOppositeDirection dir) (getTurnDirection dir) visited
                                          | Set.member (point, dir) visited = False
                                          | otherwise = isFinite rows maxX maxY (getNextPoint point dir) dir (Set.insert (point, dir) visited)
    where (x, y) = point

addObstacle :: [String] -> Int -> Int -> [String]
addObstacle rows x y = rows1 ++ (cols1 ++ '#' : cols2) : rows2
    where row = rows !! y
          (rows1, _:rows2) = splitAt y rows
          (cols1, _:cols2) = splitAt x row

countInfinitePossibilities :: [(Int, Int)] -> [String] -> Int -> Int -> (Int, Int) -> Int
countInfinitePossibilities [] _ _ _ _ = 0
countInfinitePossibilities (p:ps) rows maxX maxY point = (fromEnum $ not $ isFinite (addObstacle rows x y) maxX maxY point 0 Set.empty) + countInfinitePossibilities ps rows maxX maxY point
    where (x, y) = p

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = lines content
        startPoint = getStartPoint rows 0
        maxX = length rows - 1
        maxY = length (head rows) - 1
        points = getPoints rows maxX maxY startPoint 0 Set.empty
    print $ countInfinitePossibilities points rows maxX maxY startPoint

