module Main where
import qualified Data.Map as Map

parseMapContentLine :: String -> Int -> Int -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
parseMapContentLine [] _ _ coords = coords
parseMapContentLine (c:cs) x y coords = parseMapContentLine cs (x + 1) y $ Map.insert (x, y) c coords

parseMapContent :: [String] -> Int -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
parseMapContent [] _ coords = coords
parseMapContent (l:ls) y coords = parseMapContent ls (y + 1) $ parseMapContentLine l 0 y coords

getNextPoint :: (Int, Int) -> Char -> (Int, Int)
getNextPoint (x, y) '^' = (x, y - 1)
getNextPoint (x, y) '>' = (x + 1, y)
getNextPoint (x, y) 'v' = (x, y + 1)
getNextPoint (x, y) '<' = (x - 1, y)

moveObstacle :: Map.Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Char -> (Map.Map (Int, Int) Char, Bool)
moveObstacle coords firstPoint point dir | nextObstacle == '#' = (coords, False)
                                         | nextObstacle == '.' = (Map.insert nextPoint 'O' $ Map.insert firstPoint '.' coords, True)
                                         | nextObstacle == 'O' = moveObstacle coords firstPoint nextPoint dir
    where nextPoint = getNextPoint point dir
          nextObstacle = coords Map.! nextPoint

move :: Map.Map (Int, Int) Char -> (Int, Int) -> Char -> (Map.Map (Int, Int) Char, (Int, Int))
move coords point dir | nextObstacle == '#' = (coords, point)
                      | nextObstacle == '.' = (coords, nextPoint)
                      | nextObstacle == 'O' = let (newCoords, success) = moveObstacle coords nextPoint nextPoint dir
                                              in if success then (newCoords, nextPoint) else (coords, point)
    where nextPoint = getNextPoint point dir
          nextObstacle = coords Map.! nextPoint

processMoves :: String -> Map.Map (Int, Int) Char -> (Int, Int) -> Map.Map (Int, Int) Char
processMoves [] coords _ = coords
processMoves (c:cs) coords point = processMoves cs newCoords newPoint
    where (newCoords, newPoint) = move coords point c

main :: IO ()
main = do
    mapContent <- readFile "input-map.txt"
    movesContent <- readFile "input-moves.txt"
    let coords = parseMapContent (lines mapContent) 0 Map.empty
        point = head [k | (k, v) <- Map.toList coords, v == '@']
        newCoords = processMoves (head $ lines $ movesContent) (Map.insert point '.' coords) point
        obstacles = [k | (k, v) <- Map.toList newCoords, v == 'O']
    print $ sum $ map (\(x, y) -> x + 100 * y) obstacles
