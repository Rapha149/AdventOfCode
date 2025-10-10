module Years.Year2024.Day12 (part1, part2) where

import Util.Util
import Data.List
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Region = Map Vec [Vec]

getRegion :: Map Vec Char -> Char -> [Vec] -> Region -> Region
getRegion _ _ [] region = region
getRegion points char (p:ps) region = getRegion points char (next ++ ps) (Map.insert p perimiter region)
    where (next, perimiter) = bimap (filter (`Map.notMember` region) . map fst) (map snd) $
                              partition ((== Just char) . (points Map.!?) . fst) $ map (toFst (onBoth (+) p)) [(-1, 0), (1, 0), (0, -1), (0, 1)]

findRegions :: [String] -> [Region]
findRegions input = findNext $ Map.fromList [((r, c), x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]
    where findNext :: Map Vec Char -> [Region]
          findNext points | Map.null points = []
                          | otherwise = let (point, char) = Map.findMin points
                                            region = getRegion points char [point] Map.empty
                                        in region : findNext (Map.difference points region)

part1 :: Solution
part1 = V . sum . map (\r -> Map.size r * Map.foldr ((+) . length) 0 r) . findRegions


getSideCount :: Region -> Int
getSideCount r | Map.null region = 0
               | otherwise = 1 + getSideCount (removeSide region [pos])
    where region = Map.filter (not . null) r
          (pos, d@(dr, dc)) = second hd $ Map.findMin region
          removeSide :: Region -> [Vec] -> Region
          removeSide region' [] = region'
          removeSide region' (p:ps) = let next = filter (\np -> Map.member np region' && d `elem` region' Map.! np) $ map (onBoth (+) p) [(-dc, dr), (dc, -dr)]
                                      in removeSide (Map.adjust (delete d) p region') (next ++ ps)

part2 :: Solution
part2 = V . sum . map (\r -> Map.size r * getSideCount r) . findRegions
