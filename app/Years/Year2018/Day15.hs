module Years.Year2018.Day15 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type State = (Set Vec, Map Vec (Bool, Int))
data Status = Ongoing | Finished | ElfDied

parseInput :: [String] -> State
parseInput input = (Map.keysSet $ Map.filter (== '.') grid, Map.mapMaybe (\case 'E' -> Just (True, 200)
                                                                                'G' -> Just (False, 200)
                                                                                _ -> Nothing) grid)
    where grid = Map.fromList [((r, c), x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]

bfs :: Set Vec -> Set Vec -> [[Vec]] -> Set Vec -> Maybe Vec
bfs _ _ [] _ = Nothing
bfs open ends (path:xs) seen | Set.member pos ends = Just $ lst $ ini path
                             | otherwise = bfs open ends (xs ++ map (: path) next) (foldr Set.insert seen next)
    where pos = hd path
          next = [pos' | delta <- [{- reading order! -} (-1, 0), (0, -1), (0, 1), (1, 0)], let pos' = onBoth (+) pos delta,
                         Set.member pos' open, Set.notMember pos' seen]

attack :: Int -> State -> Vec -> (Maybe Vec, State)
attack elfPower (open, units) pos | null targets = (Nothing, (open, units))
                                  | health > power = (Nothing, (open, Map.insert target (not elf, health - power) units))
                                  | otherwise = (Just target, (Set.insert target open, Map.delete target units))
    where (elf, _) = units Map.! pos
          power = if elf then elfPower else 3
          targets = Map.filter ((/= elf) . fst) $ Map.restrictKeys units $ Set.fromList [onBoth (+) pos delta | delta <- [(-1, 0), (0, -1), (0, 1), (1, 0)]]
          (target, health) = minimumOn swap $ Map.toList $ Map.map snd targets

combatRound :: Int -> State -> [Vec] -> (State, Status)
combatRound _ state [] = (state, Ongoing)
combatRound elfPower (open, units) (pos:xs) | null targets = ((open, units), Finished)
                                            | Set.member pos adjacent = handleAttackResult $ attack elfPower (open, units) pos
                                            | null reachable = combatRound elfPower (open, units) xs
                                            | otherwise = case bfsResult of
                                                               Nothing -> combatRound elfPower (open, units) xs
                                                               Just move -> handleAttackResult $ attack elfPower (Set.insert pos $ Set.delete move open, Map.insert move unit $ Map.delete pos units) move
    where unit@(elf, _) = units Map.! pos
          targets = Map.keys $ Map.filter ((/= elf) . fst) units
          adjacent = Set.fromList $ concatMap (\p -> [onBoth (+) p d | d <- [(-1, 0), (0, -1), (0, 1), (1, 0)]]) targets
          reachable = Set.intersection adjacent open
          bfsResult = bfs open reachable [[pos]] (Set.singleton pos)
          handleAttackResult :: (Maybe Vec, State) -> (State, Status)
          handleAttackResult (Nothing, state) = combatRound elfPower state xs
          handleAttackResult (Just dead, state) | not elf && elfPower > 3 = (state, ElfDied)
                                                | otherwise = combatRound elfPower state $ delete dead xs

getBattleOutcome :: Int -> Int -> State -> Maybe Int
getBattleOutcome elfPower i state = case status of
                                         Ongoing -> getBattleOutcome elfPower (i + 1) state'
                                         Finished -> Just $ i * sum (Map.map snd $ snd state')
                                         ElfDied -> Nothing
      where (state', status) = combatRound elfPower state $ Map.keys $ snd state

part1 :: Solution
part1 = V . fromJust . getBattleOutcome 3 0 . parseInput

part2 :: Solution
part2 input = V $ hd $ mapMaybe (\elfPower -> getBattleOutcome elfPower 0 state) [4..]
    where state = parseInput input
