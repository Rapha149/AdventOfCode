module Years.Year2018.Day21 (part1, part2) where

import Util.Util
import Data.Bits
import Data.List
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Registers = Map Int Int
data Instruction = Instruction { op :: String, a :: Int, b :: Int, c :: Int }

parseInput :: [String] -> (Int, Map Int Instruction)
parseInput (ipReg:program) | not $ "#ip " `isPrefixOf` ipReg = error "Input doesn't start with #ip instruction."
                           | otherwise = (read $ drop 4 ipReg, Map.fromList $ zip [0..] $ map (parseInstruction . words) program)
    where parseInstruction :: [String] -> Instruction
          parseInstruction (op:args) = case map read args of
                                            [a, b, c] -> Instruction {..}
                                            _ -> error "Invalid instruction."
          parseInstruction _ = error "Invalid instruction."
parseInput _ = error "Empty input."

findEqrrInstruction :: Map Int Instruction -> (Int, Int)
findEqrrInstruction instructions = second (\Instruction{..} -> if a == 0 then b else a) $ Map.findMin $ Map.filter (\Instruction{..} -> op == "eqrr" && (a == 0 || b == 0)) instructions

runProgram :: Int -> Map Int Instruction -> Int -> Registers -> Registers
runProgram ipReg program stopAt regs | stopAt == ip = regs
                                     | otherwise = case program Map.!? ip of
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
part1 input = V $ runProgram ipReg program stopAt Map.empty Map.! outReg
    where (ipReg, program) = parseInput input
          (stopAt, outReg) = findEqrrInstruction program


mostSteps :: Int -> Map Int Instruction -> Int -> Int -> Registers -> Set Int -> Int -> Int
mostSteps ipReg program stopAt outReg regs seen lastSeen | num `Set.member` seen = lastSeen
                                                         | otherwise = mostSteps ipReg program stopAt outReg (runProgram ipReg program (stopAt + 1) regs') (Set.insert num seen) num
    where regs' = runProgram ipReg program stopAt regs
          num = regs' Map.! outReg

part2 :: Solution
part2 input = V $ mostSteps ipReg program stopAt outReg Map.empty Set.empty 0
    where (ipReg, program) = parseInput input
          (stopAt, outReg) = findEqrrInstruction program
