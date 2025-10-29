module Years.Year2018.Day18 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Acre = OpenGround | Trees | Lumberyard deriving (Show, Eq, Ord)
type Area = Map Vec Acre

parseInput :: [String] -> Area
parseInput input = Map.fromList [((x, y), case c of
                                               '.' -> OpenGround
                                               '|' -> Trees
                                               '#' -> Lumberyard
                                               _ -> error "Invalid input.") | (y, row) <- zip [0..] input, (x, c) <- zip [0..] row]

oneMinute :: Area -> Area
oneMinute area = Map.mapWithKey change area
    where change :: Vec -> Acre -> Acre
          change (x, y) acre = let adjacent = mapMaybe (area Map.!?) [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
                               in case acre of
                                       OpenGround -> if length (filter (== Trees) adjacent) >= 3 then Trees else OpenGround
                                       Trees -> if length (filter (== Lumberyard) adjacent) >= 3 then Lumberyard else Trees
                                       Lumberyard -> if Trees `elem` adjacent && Lumberyard `elem` adjacent then Lumberyard else OpenGround

afterMinutes :: Int -> Area -> Area
afterMinutes n = foldr (.) id $ replicate n oneMinute

getResourceValue :: Area -> Int
getResourceValue area = length (Map.filter (== Trees) area) * length (Map.filter (== Lumberyard) area)

part1 :: Solution
part1 = V . getResourceValue . afterMinutes 10 . parseInput


getLoop :: Area -> Map Area Int -> Int -> (Int, Int)
getLoop area history i = case history Map.!? area of
                           Just j -> (j, i - j)
                           Nothing -> getLoop (oneMinute area) (Map.insert area i history) (i + 1)

part2 :: Solution
part2 input = V $ getResourceValue $ afterMinutes ((1000000000 - start) `mod` size) $ afterMinutes start area
    where area = parseInput input
          (start, size) = getLoop area Map.empty 0
