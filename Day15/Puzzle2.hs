module Main where
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict
import qualified Data.Set as Set
import Text.Regex.TDFA
import Data.Maybe
import Debug.Trace

insertCoord :: Map.Map (Int, Int) Char -> Int -> Int -> Char -> Map.Map (Int, Int) Char
insertCoord coords x y c | c == 'O' = Map.insert (x * 2, y) '[' $ Map.insert (x * 2 + 1, y) ']' coords
                         | c == '@' = Map.insert (x * 2, y) '@' $ Map.insert (x * 2 + 1, y) '.' coords
                         | otherwise = Map.insert (x * 2, y) c $ Map.insert (x * 2 + 1, y) c coords

parseMapContentLine :: String -> Int -> Int -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
parseMapContentLine [] _ _ coords = coords
parseMapContentLine (c:cs) x y coords = parseMapContentLine cs (x + 1) y $ insertCoord coords x y c

parseMapContent :: [String] -> Int -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
parseMapContent [] _ coords = coords
parseMapContent (l:ls) y coords = parseMapContent ls (y + 1) $ parseMapContentLine l 0 y coords

getNextPoint :: (Int, Int) -> Char -> (Int, Int)
getNextPoint (x, y) '^' = (x, y - 1)
getNextPoint (x, y) '>' = (x + 1, y)
getNextPoint (x, y) 'v' = (x, y + 1)
getNextPoint (x, y) '<' = (x - 1, y)

moveObstacleHorizontally :: Map.Map (Int, Int) Char -> (Int, Int) -> Char -> Char -> (Map.Map (Int, Int) Char, Bool)
moveObstacleHorizontally coords point obstacle dir | nextObstacle == '#' = (coords, False)
                                                   | nextObstacle == '.' = (Map.insert nextPoint obstacle coords, True)
                                                   | nextObstacle == '[' || nextObstacle == ']' = let (newCoords, success) = moveObstacleHorizontally coords nextPoint nextObstacle dir
                                                                                                  in if success then (Map.insert nextPoint obstacle newCoords, True) else (coords, False)
    where nextPoint = getNextPoint point dir
          nextObstacle = coords Map.Strict.! nextPoint

insertObstacleVertically :: Map.Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Map.Map (Int, Int) Char
insertObstacleVertically coords nextPoint1 nextPoint2 = Map.insert nextPoint1 '[' $ Map.insert nextPoint2 ']' coords

moveObstacleVertically :: Map.Map (Int, Int) Char -> (Int, Int) -> Char -> (Map.Map (Int, Int) Char, Bool)
moveObstacleVertically coords point dir | nextObstacle1 == '#' || nextObstacle2 == '#' = (coords, False)
                                        | nextObstacle1 == '[' = let (newCoords, success) = moveObstacleVertically coords nextPoint1 dir
                                                              in if success then (newCoords, True) else (coords, False)
                                        | nextObstacle1 == '.' && nextObstacle2 == '.' = (insertObstacleVertically coords nextPoint1 nextPoint2, True)
                                        | nextObstacle1 == '.' && nextObstacle2 == '[' = let (newCoords, success) = moveObstacleVertically coords nextPoint2 dir
                                                                                         in if success then (Map.insert (getNextPoint nextPoint2 '>') '.' $ insertObstacleVertically newCoords nextPoint1 nextPoint2, True) else (coords, False)
                                        | nextObstacle1 == ']' && nextObstacle2 == '.' = let (newCoords, success) = moveObstacleVertically coords (getNextPoint nextPoint1 '<') dir
                                                                                         in if success then (Map.insert (getNextPoint nextPoint1 '<') '.' $ insertObstacleVertically newCoords nextPoint1 nextPoint2, True) else (coords, False)
                                        | nextObstacle1 == ']' && nextObstacle2 == '[' = let (newCoords1, success1) = moveObstacleVertically coords (getNextPoint nextPoint1 '<') dir
                                                                                             (newCoords2, success2) = moveObstacleVertically newCoords1 nextPoint2 dir
                                                                                         in if success1 && success2 then (Map.insert (getNextPoint nextPoint1 '<') '.' $ Map.insert (getNextPoint nextPoint2 '>') '.' $ insertObstacleVertically  newCoords2 nextPoint1 nextPoint2, True) else (coords, False)
    where nextPoint1 = getNextPoint point dir
          nextObstacle1 = coords Map.Strict.! nextPoint1
          nextPoint2 = getNextPoint (getNextPoint point '>') dir
          nextObstacle2 = coords Map.Strict.! nextPoint2

moveObstacle :: Map.Map (Int, Int) Char -> (Int, Int) -> Char -> Char -> (Map.Map (Int, Int) Char, Bool)
moveObstacle coords point obstacle dir | dir == '<' || dir == '>' = let (newCoords, success) = moveObstacleHorizontally coords point obstacle dir
                                                                    in if success then (Map.insert point '.' newCoords, True) else (coords, False)
                                       | otherwise = let normPoint = if obstacle == '[' then point else getNextPoint point '<'
                                                         (newCoords, success) = moveObstacleVertically coords normPoint dir
                                                     in if success then (Map.insert normPoint '.' $ Map.insert (getNextPoint normPoint '>') '.' newCoords, True) else (coords, False)

move :: Map.Map (Int, Int) Char -> (Int, Int) -> Char -> (Map.Map (Int, Int) Char, (Int, Int))
move coords point dir | nextObstacle == '#' = (coords, point)
                      | nextObstacle == '.' = (coords, nextPoint)
                      | nextObstacle == '[' || nextObstacle == ']' = let (newCoords, success) = moveObstacle coords nextPoint nextObstacle dir
                                                                     in if success then (newCoords, nextPoint) else (coords, point)
    where nextPoint = getNextPoint point dir
          nextObstacle = coords Map.Strict.! nextPoint

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
        obstacles = [k | (k, v) <- Map.toList newCoords, v == '[']
    print $ sum $ map (\(x, y) -> x + 100 * y) obstacles
