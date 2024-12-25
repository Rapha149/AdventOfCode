module Main where
import qualified Data.Set as Set
import qualified Data.Map as Map

data ParseState = ParseState { obstacles :: [(Int, Int)], start :: (Int, Int), end :: (Int, Int) }

parseContent :: [String] -> ParseState
parseContent ls = foldl parseLine (ParseState { obstacles = [], start = (-1, -1), end = (-1, -1) }) $ zip ls [0..]
    where parseLine state (line, y) = foldl parseChar state $ zip line [0..]
             where parseChar state (c, x) | c == '#' = state { obstacles = (x, y) : obstacles state }
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

getCombinations :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
getCombinations xs = [(a, b) | (a:bs) <- tails xs, b <- a:bs]
    where tails [] = []
          tails xs@(_:xs') = xs : tails xs'

getCheats :: Set.Set (Int, Int) -> [(Int, Int)] -> Map.Map (Int, Int) Int -> Int
getCheats _ [] _ = 0
getCheats obstacles (o:os) path = length (filter (\(c1, c2) -> abs (path Map.! c1 - path Map.! c2) >= 102) combinations) + getCheats obstacles os path
    where neighbors = filter (\p -> Map.member p path) $ getNeighbors o
          combinations = filter (\(c1, c2) -> c1 /= c2) $ getCombinations neighbors

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ParseState { obstacles, start, end } = parseContent (lines content)
        obstacleSet = Set.fromList obstacles
        path = Map.fromList $ zip (findInitialPath obstacleSet start start end) [0..]
    print $ getCheats obstacleSet obstacles path
