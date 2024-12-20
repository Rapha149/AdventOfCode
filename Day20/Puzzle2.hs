module Main where
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

data ParseState = ParseState { obstacles :: Set.Set (Int, Int), start :: (Int, Int), end :: (Int, Int) }

parseContent :: [String] -> ParseState
parseContent ls = foldl parseLine (ParseState { obstacles = Set.empty, start = (-1, -1), end = (-1, -1) }) $ zip ls [0..]
    where parseLine state (line, y) = foldl parseChar state $ zip line [0..]
            where parseChar state (c, x) | c == '#' = state { obstacles = Set.insert (x, y) $ obstacles state }
                                    | c == 'S' = state { start = (x, y) }
                                    | c == 'E' = state { end = (x, y) }
                                    | otherwise = state

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

findInitialPath :: Set.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
findInitialPath obstacles current previous end | nextPoint == end = [nextPoint]
                                               | current == previous = previous : nextPoint : findInitialPath obstacles nextPoint previous end
                                               | otherwise = nextPoint : findInitialPath obstacles nextPoint current end
    where getNextPoint (p:ps) = if p /= previous && Set.notMember p obstacles then p else getNextPoint ps
          nextPoint = getNextPoint $ getNeighbors current

getCheats :: [((Int, Int), Int)] -> [((Int, Int), (Int, Int), Int, Int)]
getCheats [] = []
getCheats ((p,n):ps) = getCheatsForPoint ps ++ getCheats ps
    where getCheatsForPoint [] = []
          getCheatsForPoint ((o,n'):os) | distance <= 20 && diff >= 100 = (p, o, distance, diff) : getCheatsForPoint os
                                        | otherwise = getCheatsForPoint os
            where distance = abs (fst p - fst o) + abs (snd p - snd o)
                  diff = abs (n' - n) - distance

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ParseState { obstacles, start, end } = parseContent (lines content)
        path = zip (findInitialPath obstacles start start end) [0..]
    print $ length $ getCheats path
