module Main where
import Data.Maybe
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

parseContent :: [String] -> [(Int, Int)]
parseContent [] = []
parseContent (l:ls) = (x, y) : parseContent ls
    where [x, y] = map read $ splitOn "," l

size = 70 :: Int

getNeighbors :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int) -> [((Int, Int), Set.Set (Int, Int))]
getNeighbors obstacles (x, y) path = map (\p -> (p, Set.insert p path)) $ filter (\(x, y) -> Set.notMember (x, y) obstacles && x >= 0 && x <= size && y >= 0 && y <= size) [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

bfs :: Set.Set (Int, Int) -> Seq.Seq ((Int, Int), Set.Set (Int, Int)) -> Set.Set (Int, Int) -> Maybe (Set.Set (Int, Int))
bfs _ Seq.Empty _ = Nothing
bfs obstacles ((current, path) Seq.:<| rest) visited | current == (size, size) = Just path
                                                     | Set.member current visited = bfs obstacles rest visited
                                                     | otherwise = let neighbors = getNeighbors obstacles current path
                                                                       newQueue = foldl' (Seq.|>) rest neighbors
                                                                   in bfs obstacles newQueue $ Set.insert current visited

findFirstBlockingObstacle :: [(Int, Int)] -> Set.Set (Int, Int) -> Set.Set (Int, Int) -> (Int, Int)
findFirstBlockingObstacle [] _ _ = error "No blocking coord found."
findFirstBlockingObstacle (p:ps) obstacles lastPath | Set.notMember p lastPath = findFirstBlockingObstacle ps newObstacles lastPath
                                                    | otherwise = let path = bfs newObstacles (Seq.singleton ((0,0), Set.empty)) Set.empty
                                                                  in if path == Nothing then p else findFirstBlockingObstacle ps newObstacles $ fromJust path
    where newObstacles = Set.insert p obstacles

main :: IO ()
main = do
    content <- readFile "input.txt"
    let coords = parseContent $ lines content
    print $ findFirstBlockingObstacle coords Set.empty $ Set.fromList coords
