module Year2021.Day19 (part1, part2) where

import Prelude hiding ((<>))
import Util
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Foldable as F
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Numeric.LinearAlgebra as LA

minMatches, pairMatches :: Int
minMatches = 12
pairMatches = (minMatches * (minMatches - 1)) `div` 2

data Scanner = Scanner { beacons :: [Vector Double], distances :: Map [Double] (Vector Double, Vector Double),
                         location :: Vector Double, rotation :: Matrix Double } deriving Show

rotations :: [Matrix Double]
rotations = nub [pow rx x <> pow ry y <> pow rz z | x <- [0..3], y <- [0..3], z <- [0..3]]
    where rx = (3><3) [0, 0, -1, 0, 1, 0, 1, 0, 0]
          ry = (3><3) [1, 0, 0, 0, 0, -1, 0, 1, 0]
          rz = (3><3) [0, -1, 0, 1, 0, 0, 0, 0, 1]
          pow :: Matrix Double -> Int -> Matrix Double
          pow m 0 = ident $ rows m
          pow m n = foldr1 (<>) $ replicate n m

parseScanner :: [String] -> Scanner
parseScanner ls = Scanner { location = vector [], rotation = matrix 0 [], .. }
    where beacons = map (vector . map read . splitOn ",") ls
          distances = Map.fromList [(sort $ toList ((v1 - v2) ** 2), (v1, v2)) | (v1:xs) <- tails beacons, v2 <- xs]

overlapping :: [Scanner] -> Map (Int, Int) [[Double]]
overlapping scanners = Map.fromList [((i1, i2), common) | ((i1, s1):xs) <- tails $ zip [0..] scanners, (i2, s2) <- xs,
                                                          let common = Map.keys $ Map.intersection s1.distances s2.distances,
                                                          length common >= pairMatches]

scannerLocation :: (Vector Double, Vector Double) -> (Vector Double, Vector Double) -> Maybe (Vector Double)
scannerLocation (a, b) (c, d) | loc1 == loc2 = Just loc1
                              | loc3 == loc4 = Just loc3
                              | otherwise = Nothing
    where (loc1, loc2) = (a - c, b - d)
          (loc3, loc4) = (a - d, b - c)

determineLocations :: Seq Scanner -> Map (Int, Int) [[Double]] -> Set Int -> Set (Vector Double) -> (Seq Scanner, Set (Vector Double))
determineLocations scanners overlaps unprocessed beacons | Set.null unprocessed = (scanners, beacons)
                                                         | otherwise = determineLocations scanners' overlaps' unprocessed' beacons'
    where ((id1, id2), dist) = hd [(if b1 then (i2, i1) else (i1, i2), d) | ((i1, i2), d) <- Map.toList overlaps,
                                    let (b1, b2) = both (`Set.member` unprocessed) (i1, i2), b1 /= b2]
          (s1, s2) = both (Seq.index scanners) (id1, id2)
          (pair1, pair2) = both ((Map.! hd dist) . distances) (s1, s2)
          r1 = both (s1.rotation #>) pair1
          (location, rotation) = hd [(s1.location + fromJust loc, r) | r <- rotations, let r2 = both (r #>) pair2,
                                                                       let loc = scannerLocation r1 r2, isJust loc]
          scanners' = Seq.update id2 (s2 { location = location, rotation = rotation }) scanners
          overlaps' = Map.delete (min id1 id2, max id1 id2) overlaps
          unprocessed' = Set.delete id2 unprocessed
          beacons' = foldr (Set.insert . (+) location . (rotation #>)) beacons s2.beacons

inputToLocations :: [String] -> (Seq Scanner, Set (Vector Double))
inputToLocations input = determineLocations (s0 Seq.<| Seq.fromList (tl scanners)) overlaps (Set.fromList [1..length scanners - 1]) (Set.fromList s0.beacons)
    where scanners = map (parseScanner . tl ) $ split null input
          overlaps = overlapping scanners
          s0 = (hd scanners) { location = vector [0, 0, 0], rotation = ident 3 }

part1 :: Solution
part1 = V . Set.size . snd . inputToLocations

part2 :: Solution
part2 input = let locations = map location $ F.toList $ fst $ inputToLocations input
              in V $ maximum [round $ sumElements $ cmap abs $ l2 - l1 | (l1:xs) <- tails locations, l2 <- xs]
