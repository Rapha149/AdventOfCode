module Years.Year2023.Day13 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Maybe

getReflectionRow :: [String] -> [String] -> Maybe Int
getReflectionRow [] _ = Nothing
getReflectionRow [_] _ = Nothing
getReflectionRow (r:rs) ms | all (uncurry (==)) $ zip rs (r:ms) = Just $ length ms + 1
                           | otherwise = getReflectionRow rs (r:ms)

getReflectionNum :: ([String] -> [String] -> Maybe Int) -> [String] -> Int
getReflectionNum getReflection rows | isJust row = 100 * fromJust row
                                    | isJust col = fromJust col
                                    | otherwise = error "No reflection."
    where row = getReflection rows []
          col = getReflection (transpose rows) []

part1 :: Solution
part1 = V . sum . map (getReflectionNum getReflectionRow) . split null


getReflectionRowSmudge :: [String] -> [String] -> Maybe Int
getReflectionRowSmudge [] _ = Nothing
getReflectionRowSmudge [_] _ = Nothing
getReflectionRowSmudge (r:rs) ms | length (filter (== 1) differences) == 1 && all (<= 1) differences = Just $ length ms + 1
                                 | otherwise = getReflectionRowSmudge rs (r:ms)
    where differences = zipWith ((length . filter not) .: zipWith (==)) rs (r:ms)

part2 :: Solution
part2 = V . sum . map (getReflectionNum getReflectionRowSmudge) . split null
