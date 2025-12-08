module Years.Year2025.Day08 (part1, part2) where

import Util.Util
import Data.Ord
import Data.List.Extra
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set

getDistance :: Vec3 -> Vec3 -> Int
getDistance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ two + (y1 - y2) ^ two + (z1 - z2) ^ two
    where two :: Int = 2

getConnections :: [String] -> [(Vec3, Vec3)]
getConnections input = sortOn (uncurry getDistance) [(a, b) | (a:bs) <- tails boxes, b <- bs]
    where boxes = map (triple . map read . splitOn ",") input

addConnection :: (Vec3, Vec3) -> [Set Vec3] -> [Set Vec3]
addConnection (a, b) circuits = case cs of
                                     [] -> Set.fromList [a, b] : circuits
                                     [c1, c2] -> Set.union c1 c2 : rest
                                     [c] -> Set.insert a (Set.insert b c) : rest
                                     _ -> error "Unexpected: more than two circuits which include a or b."
    where (cs, rest) = partition (\c -> a `Set.member` c || b `Set.member` c) circuits

part1 :: Solution
part1 raw = V $ product $ take 3 $ sortOn Down $ map length circuits
    where (k, input) = getExtraInt 1000 raw
          circuits = foldr addConnection [] $ take k $ getConnections input


getLastConnection :: Int -> [Set Vec3] -> [(Vec3, Vec3)] -> (Vec3, Vec3)
getLastConnection _ _ [] = error "Unexpected: empty list."
getLastConnection n circuits (x:xs) = case addConnection x circuits of
                                         [c] | length c == n -> x
                                         cs -> getLastConnection n cs xs

part2 :: Solution
part2 input = V $ uncurry (*) $ both fst3 $ getLastConnection (length input) [] $ getConnections input
