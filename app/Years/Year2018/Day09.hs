module Years.Year2018.Day09 (part1, part2) where

import Util.Util
import Data.Tuple.Extra
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

parseInput :: [String] -> (Int, Int)
parseInput [line] = both read $ (hd &&& (!! 6)) $ words line
parseInput _ = error "Invalid input."

play :: Seq Int -> Int -> [Int] -> [Int] -> IntMap Int
play _ _ [] _ = error "No players."
play _ _ _ [] = IM.empty
play marbles current (p:ps) (m:ms) | m `mod` 23 == 0 = IM.insertWith (+) p (m + Seq.index marbles idx) $ play (Seq.deleteAt idx marbles) idx ps ms
    where idx = (current - 7) `mod` length marbles
play marbles current (_:ps) (m:ms) = play (Seq.insertAt idx m marbles) idx ps ms
    where idx = (current + 2) `mod` length marbles

getHighestScore :: Int -> Int -> Int
getHighestScore players marbles = maximum $ play (Seq.singleton 0) 0 (cycle [1..players]) [1..marbles]

part1 :: Solution
part1 = V . uncurry getHighestScore . parseInput

part2 :: Solution
part2 = V . uncurry getHighestScore . second (* 100) . parseInput
