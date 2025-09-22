module Years.Year2021.Day09 (part1, part2) where

import Util.Util
import Data.Char
import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> Map Vec Int
parseInput input = Map.fromList [((r, c), digitToInt x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]

getLowPoints :: Map Vec Int -> Map Vec Int
getLowPoints heightmap = Map.filterWithKey (\p h -> all (maybe True (> h) . (Map.!?) heightmap . onBoth (+) p) [(1, 0), (-1, 0), (0, 1), (0, -1)]) heightmap

part1 :: Solution
part1 = V . Map.foldr ((+) . (+1)) 0 . getLowPoints . parseInput


getBasin :: Map Vec Int -> Set Vec -> [Vec] -> Set Vec
getBasin _ visited [] = visited
getBasin heightmap visited (pos:ps) = getBasin heightmap (Set.insert pos visited) (next ++ ps)
    where height = heightmap Map.! pos
          next = filter (\p -> Set.notMember p visited && maybe False (> height) (heightmap Map.!? p)) $ map (onBoth (+) pos) [(1, 0), (-1, 0), (0, 1), (0, -1)]

part2 :: Solution
part2 input = let heightmap = parseInput input
                  basins = map (getBasin (Map.filter (< 9) heightmap) Set.empty . singleton) $ Map.keys $ getLowPoints heightmap
              in V $ product $ take 3 $ sortOn Down $ map Set.size basins
