module Years.Year2023.Day24 (part1, part2) where

import Util.Util
import Data.Bifunctor
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple.Extra
import Data.Vec3
import qualified Numeric.LinearAlgebra as LA

type Stone2 = ((Double, Double), (Double, Double))

isIntersecting :: Double -> Double -> (Stone2, Stone2) -> Bool
isIntersecting areaMin areaMax (((px1, py1), (vx1, vy1)), ((px2, py2), (vx2, vy2))) | isNothing result || a < 0 || b < 0 = False
                                                                                    | otherwise = inBoundsBoth areaMin areaMax point
    where result = LA.linearSolve ((2 LA.>< 2) [vx1, -vx2, vy1, -vy2]) ((2 LA.>< 1) [px2 - px1, py2 - py1])
          (a, b) = pair $ concat $ LA.toLists $ fromJust result
          point = onBoth (+) (px1, py1) $ both (*a) (vx1, vy1)

part1 :: Solution
part1 raw = V $ length $ filter (isIntersecting (fromIntegral areaMin) (fromIntegral areaMax)) pairs
    where ((areaMin, areaMax), input) = getExtraInts 2 pair (200000000000000, 400000000000000) raw
          stones = map (pair . map (pair . take 2 . map read . splitOn ", ") . splitOn " @ ") input
          pairs = [(s1, s2) | (s1:xs) <- tails stones, s2 <- xs]


toList :: (Double, Double, Double) -> [Double]
toList (a, b, c) = [a, b, c]

part2 :: Solution
part2 input = V $ sum $ map round $ toList r
    where stones = map (pair . map (triple . map read . splitOn ", ") . splitOn " @ ") input
          (p0, v0) = hd stones -- use first stone as reference
          shifted = map (bimap (<-> p0) (<-> v0)) stones -- shift all stones relative to the first
          n1 = uncurry (><) $ shifted !! 1 -- plane between origin (first stone) and second stone
          (p2, v2) = shifted !! 2
          (p3, v3) = shifted !! 3
          t2 = -(n1 .* p2 / n1 .* v2) -- solve for t: n1 · (p2 + v2 * t) = 0
          x2 = p2 <+> v2 .^ t2 -- intersection with plane
          t3 = -(n1 .* p3 / n1 .* v3)
          x3 = p3 <+> v3 .^ t3
          vShift = (x3 <-> x2) .^ (1 / (t3 - t2)) -- rock velocity in shifted view
          (idx, val) = maximumBy (compare `on` abs . snd) $ zip [0..] $ toList vShift -- get the value in vShift farthest away from zero
          tShift = t2 - (toList x2 !! idx) / val -- time the rock passes through the origin in shifted view
          v = vShift <+> v0 -- rock velocity
          r = (p0 <+> v0 .^ tShift) <-> v .^ tShift
