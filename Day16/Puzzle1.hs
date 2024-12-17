module Main where
import Data.Maybe
import Data.Time (getCurrentTime, diffUTCTime)
import qualified Data.Set as Set
import qualified Data.PriorityQueue.FingerTree as PQ

parseContentLine :: String -> Int -> Int -> Set.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> (Set.Set (Int, Int), (Int, Int), (Int, Int))
parseContentLine [] _ _ obstacles start end = (obstacles, start, end)
parseContentLine (c:cs) x y obstacles start end | c == '#' = parseContentLine cs (x + 1) y (Set.insert (x, y) obstacles) start end
                                                | c == 'S' = parseContentLine cs (x + 1) y obstacles (x, y) end
                                                | c == 'E' = parseContentLine cs (x + 1) y obstacles start (x, y)
                                                | otherwise = parseContentLine cs (x + 1) y obstacles start end

parseContent :: [String] -> Int -> Set.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> (Set.Set (Int, Int), (Int, Int), (Int, Int))
parseContent [] _ obstacles start end = (obstacles, start, end)
parseContent (l:ls) y obstacles start end = (\(o, s, e) -> parseContent ls (y + 1) o s e) $ parseContentLine l 0 y obstacles start end

getNeighbor :: (Int, Int) -> Int -> ((Int, Int), Int)
getNeighbor (x, y) 0 = ((x, y - 1), 0)
getNeighbor (x, y) 1 = ((x + 1, y), 1)
getNeighbor (x, y) 2 = ((x, y + 1), 2)
getNeighbor (x, y) 3 = ((x - 1, y), 3)

getNeighbors :: Set.Set (Int, Int) -> (Int, Int) -> Int -> [((Int, Int), Int)]
getNeighbors obstacles current dir = filter (\(p, _) -> Set.notMember p obstacles) $ map (getNeighbor current) [dir, (dir - 1) `mod` 4, (dir + 1) `mod` 4]

type AoCQueue = PQ.PQueue Int ((Int, Int), Int)

addToQueue :: Int -> Int -> ((Int, Int), Int) -> AoCQueue -> AoCQueue
addToQueue cost prevDir (nextPos, newDir) queue = PQ.insert (cost + 1 + turnCost) (nextPos, newDir) queue
    where turnCost = if prevDir == newDir then 0 else 1000

bfs :: Set.Set (Int, Int) -> (Int, Int) -> AoCQueue -> Set.Set ((Int, Int), Int) -> Maybe Int
bfs obstacles end queue visited | PQ.null queue = Nothing
                                | current == end = Just cost
                                | Set.member (current, dir) visited = bfs obstacles end rest visited
                                | otherwise = let neighbors = getNeighbors obstacles current dir
                                                  newQueue = foldr (addToQueue cost dir) rest neighbors
                                              in bfs obstacles end newQueue $ Set.insert (current, dir) visited
    where Just ((cost, (current, dir)), rest) = PQ.minViewWithKey queue

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (obstacles, start, end) = parseContent (lines content) 0 Set.empty (-1, -1) (-1, -1)
    startTime <- getCurrentTime
    print $ bfs obstacles end (PQ.singleton 0 (start, 1)) Set.empty
    endTime <- getCurrentTime
    print $ (realToFrac (diffUTCTime endTime startTime) :: Double) * 1000
