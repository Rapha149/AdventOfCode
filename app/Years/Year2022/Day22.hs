module Years.Year2022.Day22 (part1, part2) where

import Util.Util
import Data.Char
import Data.List.Extra
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type WrapFunction = Map Vec Bool -> (Vec, Vec) -> (Vec, Vec)

parseInput :: [String] -> (Map Vec Bool, Vec, String)
parseInput input = (grid, minimum $ filter ((== 0) . fst) $ Map.keys grid, lst input)
    where grid = Map.fromList [((r, c), x == '.') | (r, row) <- zip [0..] $ dropEnd 2 input, (c, x) <- zip [0..] row, x /= ' ']

move :: Map Vec Bool -> WrapFunction -> (Vec, Vec) -> String -> (Vec, Vec)
move _ _ state [] = state
move grid wrap (pos, (dr, dc)) ('L':ms) = move grid wrap (pos, (-dc, dr)) ms
move grid wrap (pos, (dr, dc)) ('R':ms) = move grid wrap (pos, (dc, -dr)) ms
move grid wrap state ms = move grid wrap (moveN state $ read m) ms'
    where (m, ms') = span isDigit ms
          moveN :: (Vec, Vec) -> Int -> (Vec, Vec)
          moveN s 0 = s
          moveN (pos, dir) n = let p = onBoth (+) pos dir
                                   (pW, dW) = if Map.member p grid then (p, dir) else wrap grid (pos, dir)
                               in if grid Map.! pW then moveN (pW, dW) $ n - 1 else (pos, dir)

wrapEdge :: WrapFunction
wrapEdge grid (pos, dir) = ((if uncurry (+) dir > 0 then minimum else maximum) $ filter ((== x) . sel) $ Map.keys grid, dir)
    where sel = if fst dir == 0 then fst else snd
          x = sel pos

getResult :: (Vec, Vec) -> Int
getResult ((r, c), (dr, dc)) = 1000 * (r + 1) + 4 * (c + 1) + if dr + dc > 0 then dr else -(3 * dr + 2 * dc)

part1 :: Solution
part1 input = let (grid, start, moves) = parseInput input
              in V $ getResult $ move grid wrapEdge (start, (0, 1)) moves


type Edge = (Vec, Vec)

getFace :: Int -> Vec -> Vec
getFace len = both (`div` len)

edgeBfs :: (Vec -> Bool) -> Set Edge -> [(Edge, Edge)] -> Map Edge Edge -> Map Edge Edge
edgeBfs _ _ [] _ = error "For some edges no neighbor was found."
edgeBfs bounds edges ((start@(face, _), edge@(pos, (dr, dc))):xs) neighbors
                | Set.null edges = neighbors
                | Map.member start neighbors = edgeBfs bounds edges xs neighbors
                | dist > 1 && Set.member edge edges = edgeBfs bounds (Set.delete edge edges) xs (Map.insert start edge neighbors)
                | otherwise = edgeBfs bounds edges (xs ++ map (start, ) next) neighbors
    where dist = uncurry (+) $ onBoth (abs .: (-)) face pos
          next = filter (bounds . fst) $ map (onBoth (+) pos &&& id) [(dr, dc), (-dc, dr), (dc, -dr)]

wrapCube :: Int -> Map Edge Edge -> WrapFunction
wrapCube len neighbors _ (pos, dir) = (newPos, newDir)
    where select :: Bool -> (a, a) -> a
          select bool (a, b) = if bool then a else b
          (newFace, newDir) = neighbors Map.! (getFace len pos, dir)
          (fstOld, fstNew) = (fst dir == 0, fst newDir == 0)
          setFaceEnter = if uncurry (+) newDir > 0 then id else onBoth subtract $ both (* (len-1)) newDir
          newFacePos = setFaceEnter $ both (* len) newFace
          rel = select fstOld (fst, snd) pos `mod` len
          newRel | dir == newDir = rel
                 | fstOld /= fstNew && select fstOld (snd, fst) pos `mod` len == select fstNew (snd, fst) newFacePos `mod` len = rel
                 | otherwise = len - 1 - rel
          newPos = select fstNew (first, second) (+ newRel) newFacePos

part2 :: Solution
part2 input = let (grid, start, moves) = parseInput input
                  len = floor $ sqrt (fromIntegral $ Map.size grid `div` 6 :: Double)
                  faces = Set.map (getFace len) $ Map.keysSet grid
                  edges = [(face, dir) | face <- Set.toList faces, dir <- [(1, 0), (-1, 0), (0, 1), (0, -1)], onBoth (+) face dir `Set.notMember` faces]
                  minFace = (Set.findMin (Set.map fst faces) - 1, Set.findMin (Set.map snd faces) - 1)
                  maxFace = (Set.findMax (Set.map fst faces) + 1, Set.findMax (Set.map snd faces) + 1)
                  neighbors = edgeBfs (inBoundsI minFace maxFace) (Set.fromList $ map (second (both negate)) edges) (map (id &&& (\(p, d) -> (onBoth (+) p d, d))) edges) Map.empty
              in V $ getResult $ move grid (wrapCube len neighbors) (start, (0, 1)) moves
