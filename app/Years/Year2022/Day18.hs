module Years.Year2022.Day18 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set

getSurface :: Set Vec3 -> Vec3 -> Int
getSurface cubes cube = sum [1 | f <- [first3, second3, third3], d <- [-1, 1], f (+d) cube `Set.notMember` cubes]

part1 :: Solution
part1 input = V $ sumOn' (getSurface $ Set.fromList cubes) cubes
    where cubes = map (triple . map read . splitOn ",") input


getExteriorSurface :: Set Vec3 -> (Vec3, Vec3) -> [Vec3] -> Set Vec3 -> Int
getExteriorSurface _ _ [] _ = 0
getExteriorSurface lava bounds@((minX, minY, minZ), (maxX, maxY, maxZ)) (pos:ps) visited = length lavaDrops + getExteriorSurface lava bounds (next ++ ps) (foldr Set.insert visited next)
    where (lavaDrops, next) = partition (`Set.member` lava) [p | f <- [first3, second3, third3], d <- [-1, 1], let p@(x, y, z) = f (+d) pos,
                                                                 x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ,
                                                                 Set.notMember p visited]

part2 :: Solution
part2 input = V $ getExteriorSurface (Set.fromList cubes) bounds [start] (Set.singleton start)
    where cubes = map (triple . map read . splitOn ",") input
          (xs, ys, zs) = unzip3 cubes
          bounds = ((minimum xs - 1, minimum ys - 1, minimum zs - 1), (maximum xs + 1, maximum ys + 1, maximum zs + 1))
          start = fst bounds
