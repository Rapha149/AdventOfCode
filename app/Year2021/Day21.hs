module Year2021.Day21 (part1, part2) where

import Util
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Player = (Int, Int)
type State = (Bool, (Player, Player))

parseInput :: [String] -> (Player, Player)
parseInput = pair . map ((, 0) . subtract 1 . read . lst . words)

play1 :: Int -> Player -> Player -> Int
play1 rolls (pos, points) p2 | points' >= 1000 = snd p2 * rolls'
                                | otherwise = play1 rolls' p2 (pos', points')
      where forward = sum $ map ((+1) . (`mod` 1000) . (+ rolls)) [0, 1, 2]
            pos' = (pos + forward) `mod` 10
            points' = points + pos' + 1
            rolls' = rolls + 3

part1 :: Solution
part1 = V . uncurry (play1 0) . parseInput


combinations :: [(Int, Int)]
combinations = Map.toList $ Map.fromListWith (+) [(d1 + d2 + d3, 1) | d1 <- [1..3], d2 <- [1..3], d3 <- [1..3]]

play2 :: Map State (Int, Int) -> Bool -> (Player, Player) -> ((Int, Int), Map State (Int, Int))
play2 states turn players = case states Map.!? (turn, players) of
                                 Just ws -> (ws, states)
                                 Nothing -> (wins, Map.insert (turn, players) wins states')
      where (pos, points) = (if turn then fst else snd) players
            (wins, states') = foldr (\combo (accResult, accStates) -> first (onBoth (+) accResult) $ outcome accStates combo) ((0, 0), states) combinations
            outcome :: Map State (Int, Int) -> (Int, Int) -> ((Int, Int), Map State (Int, Int))
            outcome s (forward, universes) = let pos' = (pos + forward) `mod` 10
                                                 points' = points + pos' + 1
                                                 players' = if turn then ((pos', points'), snd players) else (fst players, (pos', points'))
                                                 result | points' >= 21 = (if turn then (universes, 0) else (0, universes), s)
                                                        | otherwise = first (both (* universes)) $ play2 s (not turn) players'
                                             in result

part2 :: Solution
part2 = V . uncurry max . fst . play2 Map.empty True . parseInput
