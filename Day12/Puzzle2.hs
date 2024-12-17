module Main where
import Data.Char
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict
import qualified Data.Set as Set

parseContentLine :: String -> Int -> Int -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
parseContentLine [] _ _ coords = coords
parseContentLine (c:cs) x y coords = parseContentLine cs (x + 1) y $ Map.insert (x, y) c coords

parseContent :: [String] -> Int -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
parseContent [] _ coords = coords
parseContent (l:ls) y coords = parseContent ls (y + 1) $ parseContentLine l 0 y coords

getNextPoint :: (Int, Int) -> Int -> (Int, Int)
getNextPoint point dir | dir == 0 = (x, y - 1)
                       | dir == 1 = (x + 1, y)
                       | dir == 2 = (x, y + 1)
                       | dir == 3 = (x - 1, y)
                       | otherwise = point
    where (x, y) = point

followRegion :: Map.Map (Int, Int) Char -> Char -> (Int, Int) -> Int -> Set.Set (Int, Int) -> (Map.Map (Int, Int) Char, Set.Set (Int, Int))
followRegion coords _ _ 4 region = (coords, region)
followRegion coords char point dir region | nextCharMaybe /= (Just char) || Set.member nextPoint region = followRegion coords char point (dir + 1) region
                                          | otherwise = (\(newCoords, newRegion) -> followRegion newCoords char point (dir + 1) newRegion) $ getRegion (Map.delete nextPoint coords) char nextPoint $ Set.insert nextPoint region
    where nextPoint = getNextPoint point dir
          nextCharMaybe = Map.lookup nextPoint coords

getRegion :: Map.Map (Int, Int) Char -> Char -> (Int, Int) -> Set.Set (Int, Int) -> (Map.Map (Int, Int) Char, Set.Set (Int, Int))
getRegion coords char point region = followRegion coords char point 0 region

getRegions :: Map.Map (Int, Int) Char -> [Set.Set (Int, Int)] -> [Set.Set (Int, Int)]
getRegions coords regions | Map.null coords = regions
                          | otherwise = let (point, char) = Map.findMin coords
                                        in (\(newCoords, region) -> getRegions newCoords (region:regions)) $ getRegion (Map.delete point coords) char point $ Set.singleton point

getDirPerimiter :: Set.Set (Int, Int) -> [(Int, Int)] -> Int -> Set.Set (Int, Int)
getDirPerimiter _ [] _ = Set.empty
getDirPerimiter region (p:ps) dir | Set.member (getNextPoint p dir) region = getDirPerimiter region ps dir
                                  | otherwise = Set.insert p $ getDirPerimiter region ps dir

removeSide :: Set.Set (Int, Int) -> (Int, Int) -> Int -> Set.Set (Int, Int)
removeSide perimiter point dir | Set.member point perimiter = removeSide (Set.delete point perimiter) (getNextPoint point dir) dir
                               | otherwise = perimiter

perimiterToSides :: Set.Set (Int, Int) -> Int -> Int
perimiterToSides perimiter dir | Set.null perimiter = 0
                               | otherwise = let point = Set.findMin perimiter
                                                 dir1 = (dir + 1) `mod` 4
                                                 dir2 = (dir - 1) `mod` 4
                                             in perimiterToSides (removeSide (removeSide perimiter point dir1) (getNextPoint point dir2) dir2) dir + 1

getDirSides :: Set.Set (Int, Int) -> [(Int, Int)] -> Int -> Int
getDirSides region points dir = perimiterToSides perimiter dir
    where perimiter = getDirPerimiter region points dir

getSides :: Set.Set (Int, Int) -> [(Int, Int)] -> Int
getSides region points = getDirSides region points 0 +
                         getDirSides region points 1 +
                         getDirSides region points 2 +
                         getDirSides region points 3

getPrice :: Set.Set (Int, Int) -> Int
getPrice region = length region * getSides region points
    where points = Set.toList region

main :: IO ()
main = do
    content <- readFile "input.txt"
    let coords = parseContent (lines content) 0 Map.empty
        regions = getRegions coords []
    print $ sum $ map getPrice regions
