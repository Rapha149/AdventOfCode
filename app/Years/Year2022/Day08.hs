module Years.Year2022.Day08 (part1, part2) where

import Util.Util
import Data.Char
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> Map Vec Int
parseInput input = Map.fromList [((r, c), digitToInt h) | (r, row) <- zip [0..] input, (c, h) <- zip [0..] row]

getVisibleEdge :: Map Vec Int -> Vec -> Vec -> Int -> Set Vec
getVisibleEdge heights dir point highest | Map.notMember point heights = Set.empty
                                         | highest >= height = getVisibleEdge heights dir next highest
                                         | otherwise = Set.insert point $ getVisibleEdge heights dir next height
    where height = heights Map.! point
          next = onBoth (+) point dir

part1 :: Solution
part1 input = V $ 2 * (maxR + maxC) + Set.size (foldr (\(d, p) -> Set.union $ getVisibleEdge heights' d (onBoth (+) p d) (heights Map.! p)) Set.empty starts)
    where heights = parseInput input
          maxR = length input - 1
          maxC = length (hd input) - 1
          starts = [((1, 0), (0, c)) | c <- [1..maxC-1]] ++ [((-1, 0), (maxR, c)) | c <- [1..maxC-1]] ++
                   [((0, 1), (r, 0)) | r <- [1..maxR-1]] ++ [((0, -1), (r, maxC)) | r <- [1..maxR-1]]
          heights' = Map.withoutKeys heights $ Set.fromList $ map snd starts


getVisiblePoint :: Map Vec Int -> Int -> Vec -> Vec -> Int
getVisiblePoint heights maxHeight dir point | Map.notMember point heights = 0
                                            | heights Map.! point >= maxHeight = 1
                                            | otherwise = 1 + getVisiblePoint heights maxHeight dir (onBoth (+) point dir)

getScenicScore :: Map Vec Int -> Vec -> Int
getScenicScore heights point = productOn' (\d -> getVisiblePoint heights height d $ onBoth (+) point d) [(1, 0), (-1, 0), (0, 1), (0, -1)]
    where height = heights Map.! point

part2 :: Solution
part2 input = V $ maximum $ map (getScenicScore heights) $ Map.keys heights
    where heights = parseInput input
