module Years.Year2018.Day16 (part1, part2) where

import Util.Util
import Data.Bits
import Data.Word
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Registers = Registers { r0 :: Int, r1 :: Int, r2 :: Int, r3 :: Int } deriving (Show, Eq)
data Args = Args { a :: Int, b :: Int, c :: Int } deriving Show
data Sample = Sample { op :: Int, args :: Args, before :: Registers, after :: Registers } deriving Show

parseInput :: [String] -> ([Sample], [(Int, Args)])
parseInput input = (samples, map parseInstruction programInput)
    where (sampleInput, programInput) = pair $ splitOn ["", "", ""] input
          samples = [Sample {..} | [beforeStr, instr, afterStr] <- splitOn [""] sampleInput, let (op, args) = parseInstruction instr,
                                   let before = parseRegisters beforeStr, let after = parseRegisters afterStr]
          parseRegisters :: String -> Registers
          parseRegisters line = case read $ concat $ tl $ words line of
                                     [a, b, c, d] -> Registers a b c d
                                     _ -> error "Invalid registers input."
          parseInstruction :: String -> (Int, Args)
          parseInstruction line = case map read $ words line of
                                       [op, a, b, c] -> (op, Args a b c)
                                       _ -> error "Invalid instruction input."

get :: Int -> Registers -> Int
get 0 = r0
get 1 = r1
get 2 = r2
get 3 = r3
get _ = error "Invalid register."

set :: Int -> Int -> Registers -> Registers
set 0 val regs = regs { r0 = val }
set 1 val regs = regs { r1 = val }
set 2 val regs = regs { r2 = val }
set 3 val regs = regs { r3 = val }
set _ _ _ = error "Invalid register."

instruction :: Word16 -> Args -> Registers -> Registers
instruction bits Args{..} regs = set c (case bits of
                                  {- addr -} 0x0001 -> aVal + bVal
                                  {- addi -} 0x0002 -> aVal + b
                                  {- mulr -} 0x0004 -> aVal * bVal
                                  {- muli -} 0x0008 -> aVal * b
                                  {- banr -} 0x0010 -> aVal .&. bVal
                                  {- bani -} 0x0020 -> aVal .&. b
                                  {- borr -} 0x0040 -> aVal .|. bVal
                                  {- bori -} 0x0080 -> aVal .|. b
                                  {- setr -} 0x0100 -> aVal
                                  {- seti -} 0x0200 -> a
                                  {- gtir -} 0x0400 -> fromEnum $ a > bVal
                                  {- gtri -} 0x0800 -> fromEnum $ aVal > b
                                  {- gtrr -} 0x1000 -> fromEnum $ aVal > bVal
                                  {- eqir -} 0x2000 -> fromEnum $ a == bVal
                                  {- eqri -} 0x4000 -> fromEnum $ aVal == b
                                  {- eqrr -} 0x8000 -> fromEnum $ aVal == bVal
                                             _ -> error "Invalid bitmask") regs
    where aVal = get a regs
          bVal = get b regs

getMatchingBits :: Sample -> [Word16]
getMatchingBits Sample {..} = [bits | i <- [0..15], let bits = setBit 0 i, instruction bits args before == after]

part1 :: Solution
part1 = V . length . filter ((>= 3) . length . getMatchingBits) . fst . parseInput


getOpcodeMap :: [Sample] -> Map Int Word16 -> Map Int Word16
getOpcodeMap [] masks | 0 `elem` masks = error "At least one opcode has no possibilities."
                      | null masks = Map.empty
                      | null determined = error "Some opcodes have multiple possibilities."
                      | otherwise = Map.union determined $ getOpcodeMap [] $ Map.map (.&. mask) rest
    where (determined, rest) = Map.partition ((== 1) . popCount) masks
          mask = complement $ foldr1 (.|.) determined
getOpcodeMap (sample:xs) masks = getOpcodeMap xs $ Map.adjust (.&. foldr1 (.|.) (getMatchingBits sample)) sample.op masks

part2 :: Solution
part2 input = V $ r0 $ foldl (\regs (op, args) -> instruction (opcodes Map.! op) args regs) (Registers 0 0 0 0) program
    where (samples, program) = parseInput input
          opcodes = getOpcodeMap samples (Map.fromList $ map (, 0xFFFF) [0..15])
