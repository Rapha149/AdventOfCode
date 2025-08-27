module Year2022.Day23 (part1, part2) where

import Util
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Cond = After Int | NoMoves

move :: Cond -> Int -> Set Vec -> [Vec] -> (Int, Set Vec)
move cond n elves dirs | stop = (n + 1, elves')
                       | otherwise = move cond (n + 1) elves' (tl dirs ++ [hd dirs])
    where moves = Map.map fromJust $ Map.filter isJust $ Set.foldr propose Map.empty elves
          elves' = Map.foldrWithKey (\new old -> Set.insert new . Set.delete old) elves moves
          stop = case cond of
                      After n' -> n + 1 == n'
                      NoMoves -> Map.null moves
          isFree :: Vec -> Vec -> Bool
          isFree elf (dr, dc) = all ((`Set.notMember` elves) . onBoth (+) elf) [(dr, dc), (dr - dc, dc + dr), (dr + dc, dc - dr)]
          propose :: Vec -> Map Vec (Maybe Vec) -> Map Vec (Maybe Vec)
          propose elf = case filter (isFree elf) dirs of
                             [] -> id
                             [_,_,_,_] -> id -- all surrounding positions are empty
                             (dir:_) -> Map.insertWith (const $ const Nothing) (onBoth (+) elf dir) (Just elf)

part1 :: Solution
part1 input = let elves = Set.fromList [(r, c) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row, x == '#']
                  (_, moved) = move (After 10) 0 elves [(-1, 0), (1, 0), (0, -1), (0, 1)]
                  rs = sort $ map fst $ Set.toList moved
                  cs = sort $ map snd $ Set.toList moved
              in V $ sum [1 | r <- [hd rs..lst rs], c <- [hd cs..lst cs], Set.notMember (r, c) moved]

part2 :: Solution
part2 input = let elves = Set.fromList [(r, c) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row, x == '#']
              in V $ fst $ move NoMoves 0 elves [(-1, 0), (1, 0), (0, -1), (0, 1)]
