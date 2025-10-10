module Years.Year2019.Day15 (part1, part2) where

import Util.Util
import Years.Year2019.IntcodeComputer
import Data.Set (Set)
import qualified Data.Set as Set

data Region = Region { area :: Set Vec, oxygenCost :: Int, oxygenPos :: Vec }

bfs :: [(Int, Vec, State)] -> Region -> Region
bfs [] region = region
bfs ((cost, pos, state):xs) region@Region {area} | Set.member pos area = bfs xs region
                                                 | otherwise = case state.outputs of
                                                                    [0] -> bfs xs region
                                                                    [1] -> bfs (xs ++ next) $ region { area = area' }
                                                                    [2] -> bfs (xs ++ next) $ region { area = area', oxygenCost = cost, oxygenPos = pos }
                                                                    _ -> error "Invalid output."
    where area' = Set.insert pos area
          next = [(cost + 1, onBoth (+) pos dir, run $ state { inputs = [i], outputs = [] }) | (i, dir) <- [(1, (0, 1)), (2, (0, -1)), (3, (-1, 0)), (4, (1, 0))]]

getRegion :: [String] -> Region
getRegion input = bfs [(0, (0, 0), (parseState input) { outputs = [1] })] (Region Set.empty 0 (0, 0))

part1 :: Solution
part1 = V . oxygenCost . getRegion


fill :: Set Vec -> [Vec] -> Int
fill area border | null area' = 0
                 | otherwise = 1 + fill area' border'
    where area' = Set.difference area $ Set.fromList border
          border' = concatMap (\(x, y) -> filter (`Set.member` area') [(x, y + 1), (x, y - 1), (x - 1, y), (x + 1, y)]) border

part2 :: Solution
part2 input = V $ fill area [oxygenPos]
    where Region {..} = getRegion input
