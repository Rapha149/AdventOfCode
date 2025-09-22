module Years.Year2019.Day12 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra

type Moon = ([Int], [Int])

parseInput :: [String] -> [Moon]
parseInput = map (\line -> (map (read . ini . lst . splitOn "=") $ words line, [0, 0, 0]))

move :: [Moon] -> [Moon]
move moons = [(zipWith (+) pos vel', vel') | (pos, vel) <- moons, let vel' = foldr (\(p, _) -> zipWith (+) $ zipWith (signum .: (-)) p pos) vel moons]

part1 :: Solution
part1 = V . sumOn' (uncurry (*) . both (sumOn' abs)) . (\(steps, input) -> foldr ($) (parseInput input) $ replicate steps move) . getExtraInt 1000


getCycle :: [Int] -> Int -> [Moon] -> [Int]
getCycle [] _ _ = []
getCycle coords steps moons = replicate (length zero) (steps * 2) ++ getCycle notZero (steps + 1) moons'
    where moons' = move moons
          vels = transpose $ map snd moons'
          (zero, notZero) = partition (all (== 0) . (vels !!)) coords

part2 :: Solution
part2 = V . foldr1 lcm . getCycle [0, 1, 2] 1 . parseInput
