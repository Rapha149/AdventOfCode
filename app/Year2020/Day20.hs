module Year2020.Day20 (part1, part2) where

import Util
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

data Tile = Tile { edges :: IntMap String, content :: [String], neighbors :: [Int], rotations :: Int, flipped :: Bool } deriving Show

sharedEdge :: Tile -> Tile -> [String]
sharedEdge t1 t2 = uncurry intersect $ both (IM.elems . edges) (t1, t2)

getTiles :: [String] -> IntMap Tile
getTiles input = IM.mapWithKey (\tileId tile -> tile { neighbors = IM.keys $ IM.filter (not . null . sharedEdge tile) $ IM.delete tileId tiles}) tiles
    where tiles = IM.fromList $ map parseTile $ filter (not . null) $ split null input
          parseTile :: [String] -> (Int, Tile)
          parseTile (l:grid) = let tileId = read $ ini $ drop 5 l
                                   grid' = transpose grid
                                   edges = IM.fromList $ zip [0..] [f edge | f <- [id, reverse], edge <- [hd grid, lst grid', lst grid, hd grid']]
                                   content = map (ini . tl) $ ini $ tl grid
                               in (tileId, Tile edges content [] 0 False)
          parseTile _ = error "Invalid input."

part1 :: Solution
part1 = V . product . IM.keys . IM.filter ((== 2) . length . neighbors) . getTiles


edgeStates :: [[Int]]
edgeStates = [[0, 1, 2, 3, 4, 3, 6, 1], -- 0°
              [7, 0, 5, 2, 3, 2, 1, 0], -- 90°
              [6, 7, 4, 5, 2, 5, 0, 7], -- 180°
              [1, 6, 3, 4, 5, 4, 7, 6]] -- 270°
            -- normal    | flipped

edgeAt :: Tile -> Int -> String
edgeAt Tile {..} dir = edges IM.! (edgeStates !! rotations !! idx)
    where idx = dir + if flipped then 4 else 0

arrange :: Tile -> Int -> String -> Tile
arrange tile dir edge | edgeAt tile dir == edge = tile
                      | tile.rotations < 3 = arrange (tile { rotations = tile.rotations + 1 }) dir edge
                      | otherwise = arrange (tile { rotations = 0, flipped = not tile.flipped }) dir edge

arrangeFirst :: Tile -> [String] -> Tile
arrangeFirst tile edges | edgeAt tile 1 `elem` edges && edgeAt tile 2 `elem` edges = tile
                        | tile.rotations < 3 = arrangeFirst (tile { rotations = tile.rotations + 1 }) edges
                        | otherwise = arrangeFirst (tile { rotations = 0, flipped = not tile.flipped }) edges

reassemble :: IntMap Tile -> Map Vec Tile
reassemble tiles = Map.insert (0, 0) corner' $ assembleRest [((0, 0), corner')] (IS.singleton cornerId)
    where (cornerId, corner) = IM.findMin $ IM.filter ((== 2) . length . neighbors) tiles
          shared = concatMap (sharedEdge corner . (IM.!) tiles) corner.neighbors
          corner' = arrangeFirst corner shared
          assembleRest :: [((Int, Int), Tile)] -> IntSet -> Map Vec Tile
          assembleRest [] _ = Map.empty
          assembleRest (((row, col), tile):xs) seen = Map.union (Map.fromList nextTiles) $ assembleRest (nextTiles ++ xs) (IS.union seen $ IS.fromList nextIds)
              where (eastEdge, southEdge) = both (edgeAt tile) (1, 2)
                    next = mapMaybe (\i -> (i, ) <$> getNext (tiles IM.! i)) $ filter (`IS.notMember` seen) tile.neighbors
                    (nextIds, nextTiles) = (map fst next, map snd next)
                    getNext :: Tile -> Maybe ((Int, Int), Tile)
                    getNext t | eastEdge `elem` t.edges = Just ((row, col + 1), arrange t 3 eastEdge)
                              | southEdge `elem` t.edges = Just ((row + 1, col),  arrange t 0 southEdge)
                              | otherwise = Nothing

getFinalContent :: Tile -> Set Vec
getFinalContent Tile {..} = Set.fromList [(r, c) | (r, row) <- zip [0..] content', (c, x) <- zip [0..] row, x == '#']
    where rotated = foldr ($) content $ replicate rotations (map reverse . transpose)
          content' = (if flipped then map reverse else id) rotated

getSeaMonster :: Set Vec -> Vec -> Set Vec
getSeaMonster image pos | all (`Set.member` image) monster = Set.fromList monster
                       | otherwise = Set.empty
    where monster = map (onBoth (+) pos) [(0, 0), (1, 1),
                                          (1, 4), (0, 5), (0, 6), (1, 7),
                                          (1, 10), (0, 11), (0, 12), (1, 13),
                                          (1, 16), (0, 17), (-1, 18), (0, 18), (0, 19)]

part2 :: Solution
part2 input = let tiles = getTiles input
                  width = length $ content $ snd $ IM.findMin tiles
                  assembled = Map.map getFinalContent $ reassemble tiles
                  image = Map.foldrWithKey (\pos -> Set.union . Set.map (onBoth (+) (both (* width) pos))) Set.empty assembled
                  monsters = hd [ms | f <- [id, second negate, -- 0°
                                            second negate . swap, swap, -- 90°
                                            both negate, first negate, -- 180°
                                            first negate . swap, both negate], -- 270°
                                      let image' = Set.map f image, let ms = foldr (Set.union . getSeaMonster image') Set.empty image',
                                      not $ null ms]
              in V $ Set.size image - Set.size monsters
