module Years.Year2018.Day04 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parseInput :: [String] -> Map Int [Int]
parseInput input = getSleepTimes 0 0 $ map (parseLine . words) $ sort input
    where parseLine :: [String] -> (Int, [String])
          parseLine (_:time:xs) = (read $ ini $ drop 3 time, xs)
          parseLine _ = error "Invalid input."
          getSleepTimes :: Int -> Int -> [(Int, [String])] -> Map Int [Int]
          getSleepTimes _ _ [] = Map.empty
          getSleepTimes _ _ ((_, ["Guard", guard, "begins", "shift"]):xs) = getSleepTimes (read $ tl guard) 0 xs
          getSleepTimes guard _ ((minute, ["falls", "asleep"]):xs) = getSleepTimes guard minute xs
          getSleepTimes guard asleep ((minute, ["wakes", "up"]):xs) = Map.insertWith (++) guard [asleep..minute-1] $ getSleepTimes guard 0 xs
          getSleepTimes _ _ _ = error "Unknown guard action."

getMostAsleepMinute :: [Int] -> (Int, Int)
getMostAsleepMinute = maximumOn snd . Map.toList . Map.fromListWith (+) . map (, 1)

part1 :: Solution
part1 = V . uncurry (*) . second (fst . getMostAsleepMinute) . maximumOn (length . snd) . Map.toList . parseInput

part2 :: Solution
part2 = V . uncurry (*) . second fst . maximumOn (snd . snd) . Map.toList . Map.map getMostAsleepMinute . parseInput
