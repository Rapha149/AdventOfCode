module Years.Year2023.Day16 (part1, part2) where

import Util.Util
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Beam = (Vec, Vec)

parseInput :: [String] -> (Int, Int, [Int], [Int], Map Vec Char)
parseInput input = (height, width, rows, cols, Map.fromList [((r, c), input !! r !! c) | r <- rows, c <- cols])
    where height = length input
          width = length $ hd input
          rows = [0..height - 1]
          cols = [0..width - 1]

countEnergizedTiles :: Map Vec Char -> Int -> Int -> Set Beam -> [Beam] -> Int
countEnergizedTiles _ _ _ tiles [] = Set.size $ Set.map fst tiles
countEnergizedTiles layout height width tiles ((p, (dr, dc)):bs) = countEnergizedTiles layout height width (foldr Set.insert tiles beams) (beams ++ bs)
    where beams  = filter (`Set.notMember` tiles) $ filter (inBounds0 height width . fst) $ map (toFst (onBoth (+) p)) $ case layout Map.! p of
                      '.' -> [(dr, dc)]
                      '/' -> [(-dc, -dr)]
                      '\\' -> [(dc, dr)]
                      '-' -> if dr == 0 then [(dr, dc)] else [(0, -1), (0, 1)]
                      '|' -> if dc == 0 then [(dr, dc)] else [(-1, 0), (1, 0)]
                      _ -> error "Invalid character."

part1 :: Solution
part1 input = V $ countEnergizedTiles layout height width (Set.singleton ((0, 0), (0, 1))) [((0, 0), (0, 1))]
    where (height, width, _, _, layout) = parseInput input

part2 :: Solution
part2 input = V $ maximum $ map (\x -> countEnergizedTiles layout height width (Set.singleton x) [x]) $ [((r, 0), (0, 1)) | r <- rows] ++
                                                                                                        [((r, width - 1), (0, -1)) | r <- rows] ++
                                                                                                        [((0, c), (1, 0)) | c <- cols] ++
                                                                                                        [((height - 1, c), (-1, 0)) | c <- cols]
    where (height, width, rows, cols, layout) = parseInput input
