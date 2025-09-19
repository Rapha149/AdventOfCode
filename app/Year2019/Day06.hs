module Year2019.Day06 (part1, part2) where

import Util
import Data.List.Extra
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> Map String [String]
parseInput = Map.fromListWith (++) . map (second singleton . pair . splitOn ")")

countOrbits :: Map String [String] -> Int -> String -> Int
countOrbits orbits count object = count + sumOn' (countOrbits orbits $ count + 1) (Map.findWithDefault [] object orbits)

part1 :: Solution
part1 = V . (\orbits -> countOrbits orbits 0 "COM") . parseInput


bfs :: Map String [String] -> [(String, Int)] -> Set String -> Int
bfs _ [] _ = error "No path found."
bfs _ (("SAN", len):_) _ = len
bfs neighbors ((object, len):xs) seen = bfs neighbors (xs ++ map (, len + 1) next) (Set.union seen $ Set.fromList next)
    where next = filter (`Set.notMember` seen) $ neighbors Map.! object

part2 :: Solution
part2 input = let orbits = parseInput input
                  neighbors = Map.unionWith (nub .: (++)) orbits $ Map.fromList $ concatMap (\(a, bs) -> map (, [a]) bs) $ Map.toList orbits
              in V $ bfs neighbors [("YOU", -2)] (Set.singleton "YOU")
