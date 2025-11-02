module Years.Year2018.Day19 (part1, part2) where

import Util.Util
import Data.Bits
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Registers = Map Int Int
data Instruction = Instruction { op :: String, a :: Int, b :: Int, c :: Int }

parseInput :: [String] -> (Int, [Instruction])
parseInput (ipReg:program) | not $ "#ip " `isPrefixOf` ipReg = error "Input doesn't start with #ip instruction."
                           | otherwise = (read $ drop 4 ipReg, map (parseInstruction . words) program)
    where parseInstruction :: [String] -> Instruction
          parseInstruction (op:args) = case map read args of
                                            [a, b, c] -> Instruction {..}
                                            _ -> error "Invalid instruction."
          parseInstruction _ = error "Invalid instruction."
parseInput _ = error "Empty input."

runProgram :: Int -> [Instruction] -> Maybe Int -> Registers -> Registers
runProgram ipReg program stopAt regs | stopAt == Just ip = regs
                                     | otherwise = case program !? ip of
        Nothing -> regs
        Just (Instruction {..}) -> let aVal = Map.findWithDefault 0 a regs
                                       bVal = Map.findWithDefault 0 b regs
                                       regs' = Map.insert c (case op of
                                                                  "addr" -> aVal + bVal
                                                                  "addi" -> aVal + b
                                                                  "mulr" -> aVal * bVal
                                                                  "muli" -> aVal * b
                                                                  "banr" -> aVal .&. bVal
                                                                  "bani" -> aVal .&. b
                                                                  "borr" -> aVal .|. bVal
                                                                  "bori" -> aVal .|. b
                                                                  "setr" -> aVal
                                                                  "seti" -> a
                                                                  "gtir" -> fromEnum $ a > bVal
                                                                  "gtri" -> fromEnum $ aVal > b
                                                                  "gtrr" -> fromEnum $ aVal > bVal
                                                                  "eqir" -> fromEnum $ a == bVal
                                                                  "eqri" -> fromEnum $ aVal == b
                                                                  "eqrr" -> fromEnum $ aVal == bVal
                                                                  _ -> error "Invalid operation.") regs
                                   in runProgram ipReg program stopAt $ Map.alter (Just . maybe 1 (+ 1)) ipReg regs'
    where ip = Map.findWithDefault 0 ipReg regs

part1 :: Solution
part1 input = V $ runProgram ipReg program Nothing Map.empty Map.! 0
    where (ipReg, program) = parseInput input

part2 :: Solution
part2 input = V $ sum [x | x <- [1..target], target `mod` x == 0]
    where (ipReg, program) = parseInput input
          (loopIp, tmpReg) = hd [(ip, c) | (ip, Instruction "mulr" _ _ c) <- zip [0..] program]
          targetReg = hd [if a == tmpReg then b else a | Instruction "eqrr" a b c <- program, a == tmpReg || b == tmpReg, c == tmpReg]
          target = runProgram ipReg program (Just loopIp) (Map.singleton 0 1) Map.! targetReg
