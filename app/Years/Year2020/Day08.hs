module Years.Year2020.Day08 (part1, part2) where

import Util.Util
import Data.Foldable
import Data.Tuple.Extra
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

data Instruction = Nop Int | Acc Int | Jmp Int

parseInput :: [String] -> Seq Instruction
parseInput [] = Seq.empty
parseInput (l:ls) = (case op of
                          "nop" -> Nop
                          "acc" -> Acc
                          "jmp" -> Jmp
                          _ -> error "Invalid instruction.") value Seq.<| parseInput ls
    where (op, v) = pair $ words l
          value = read $ case v of ('+':xs) -> xs; _ -> v

getAcc :: Seq Instruction -> Int -> Set Int -> (Int, Bool)
getAcc instructions i seen | Set.member i seen = (0, False)
                           | otherwise = case instructions Seq.!? i of
                                              Just (Nop _) -> getAcc instructions (i + 1) (Set.insert i seen)
                                              Just (Acc v) -> first (+ v) $ getAcc instructions (i + 1) (Set.insert i seen)
                                              Just (Jmp j) -> getAcc instructions (i + j) (Set.insert i seen)
                                              Nothing -> (0, True)

part1 :: Solution
part1 input = let instructions = parseInput input
              in V $ fst $ getAcc instructions 0 Set.empty

part2 :: Solution
part2 input = let instructions = parseInput input
              in V $ hd [acc | (i, instr) <- zip [0..] $ toList instructions,
                               case instr of Acc _ -> False; _ -> True,
                               let instr' = case instr of Nop v -> Jmp v; Jmp v -> Nop v; Acc _ -> error "Unexpected acc instruction.",
                               let (acc, finishes) = getAcc (Seq.update i instr' instructions) 0 Set.empty, finishes]
