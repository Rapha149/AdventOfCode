module Years.Year2019.Day20 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Tile = Passage | Portal Vec Bool | Start | End deriving (Show, Eq)

parseInput :: [String] -> Map Vec Tile
parseInput input = Map.union (Map.map (const Passage) passageGrid) (Map.mapWithKey getPortalTile portals)
    where grid = Map.fromList [((x, y), c) | (y, row) <- zip [0..] input, (x, c) <- zip [0..] row, c `notElem` " #"]
          (passageGrid, portalGrid) = Map.partition (== '.') grid
          ((minX, maxX), (minY, maxY)) = getBounds $ Map.keys passageGrid
          portals = parsePortals portalGrid $ Map.keysSet passageGrid
          getPortalTile :: Vec -> (String, Vec) -> Tile
          getPortalTile _ ("AA", _) = Start
          getPortalTile _ ("ZZ", _) = End
          getPortalTile pos (portal, _) = Portal (snd $ snd $ Map.findMin $ Map.filter ((== portal) . fst) $ Map.delete pos portals) (inBounds (minX, minY) (maxX, maxY) pos)

parsePortals :: Map Vec Char -> Set Vec -> Map Vec (String, Vec)
parsePortals grid passages | null grid = Map.empty
                           | otherwise = Map.insert adjacent ([c, grid Map.! other], passage) $ parsePortals (Map.delete other rest) passages
    where ((pos@(x, y), c), rest) = fromJust $ Map.minViewWithKey grid
          dir = if Map.member (x + 1, y) rest then (1, 0) else (0, 1)
          other = onBoth (+) pos dir
          (passage, adjacent) = hd [(p, adj) | (fac, adj) <- [(-1, pos), (2, other)], let p = onBoth (+) pos $ both (* fac) dir, Set.member p passages]

bfs1 :: Map Vec Tile -> [(Vec, Int)] -> Set Vec -> Int
bfs1 _ [] _ = error "No path found."
bfs1 grid ((pos, cost):xs) seen = case grid Map.! pos of
                                       End -> cost
                                       Portal p _ -> bfs1 grid ((p, cost):xs) seen
                                       _ -> bfs1 grid (xs ++ map (, cost + 1) next) (foldr Set.insert seen next)
    where next = filter (\p -> Set.notMember p seen && Map.member p grid) $ map (onBoth (+) pos) [(1, 0), (-1, 0), (0, 1), (0, -1)]

part1 :: Solution
part1 input = let grid = parseInput input
                  start = fst $ Map.findMin $ Map.filter (== Start) grid
              in V $ bfs1 grid [(start, -2)] (Set.singleton start)


bfs2 :: Map Vec Tile -> [(Vec, Int, Int)] -> Set (Vec, Int) -> Int
bfs2 _ [] _ = error "No path found."
bfs2 grid ((pos, depth, cost):xs) seen = case grid Map.! pos of
                                              End -> if depth == 0 then cost else bfs2 grid xs seen
                                              Portal p inner -> if inner || depth >= 1 then bfs2 grid ((p, depth + if inner then 1 else -1, cost):xs) seen
                                                                                       else bfs2 grid xs seen
                                              _ -> bfs2 grid (xs ++ map (, depth, cost + 1) next) (foldr (Set.insert . (, depth)) seen next)
    where next = filter (\p -> Set.notMember (p, depth) seen && Map.member p grid) $ map (onBoth (+) pos) [(1, 0), (-1, 0), (0, 1), (0, -1)]

part2 :: Solution
part2 input = let grid = parseInput input
                  start = fst $ Map.findMin $ Map.filter (== Start) grid
              in V $ bfs2 grid [(start, 0, -2)] (Set.singleton (start, 0))
