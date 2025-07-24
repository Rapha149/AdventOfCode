module Main where
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

parseContent :: [String] -> [(Int, Int)]
parseContent [] = []
parseContent (l:ls) = (x, y) : parseContent ls
    where [x, y] = map read $ splitOn "," l

size = 70 :: Int
bytes = 1024 :: Int

getNeighbors :: Set.Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
getNeighbors obstacles (x, y) = filter (\(x, y) -> Set.notMember (x, y) obstacles && x >= 0 && x <= size && y >= 0 && y <= size) [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

bfs :: Set.Set (Int, Int) -> Seq.Seq (Int, (Int, Int)) -> Set.Set (Int, Int) -> Maybe Int
bfs _ Seq.Empty _ = Nothing
bfs obstacles ((cost, current) Seq.:<| rest) visited | current == (size, size) = Just cost
                                                     | Set.member current visited = bfs obstacles rest visited
                                                     | otherwise = let neighbors = getNeighbors obstacles current
                                                                       newQueue = foldl' (Seq.|>) rest $ map ((cost + 1,)) neighbors
                                                                   in bfs obstacles newQueue $ Set.insert current visited

main :: IO ()
main = do
    content <- readFile "input.txt"
    let coords = parseContent $ lines content
        obstacles = Set.fromList $ take bytes coords
    print $ bfs obstacles (Seq.singleton (0, (0,0))) Set.empty
