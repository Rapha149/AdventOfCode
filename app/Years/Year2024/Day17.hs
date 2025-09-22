module Years.Year2024.Day17 (part1, part2) where

import Util.Util
import Data.Bits
import Data.List.Extra
import Data.Tuple.Extra

data Registers = R { a :: Int, b :: Int, c :: Int } deriving Show
type Program = [Int]

parseInput :: [String] -> (Registers, Program)
parseInput input = (uncurry3 R $ triple $ map (read . drop 12) reg,
                    map read $ splitOn "," $ drop 9 $ hd prog)
    where (reg, prog) = pair $ split null input

runProgram :: Program -> Registers -> Int -> [Int]
runProgram program registers pointer | pointer >= length program = []
                                     | otherwise = case program !! pointer of
                                                        0 -> runProgram program (registers { a = a registers `div` 2 ^ combo }) $ pointer + 2
                                                        1 -> runProgram program (registers { b = b registers `xor` literal }) $ pointer + 2
                                                        2 -> runProgram program (registers { b = combo `mod` 8 }) $ pointer + 2
                                                        3 -> runProgram program registers $ if a registers == 0 then pointer + 2 else literal
                                                        4 -> runProgram program (registers { b = b registers `xor` c registers }) $ pointer + 2
                                                        5 -> combo `mod` 8 : runProgram program registers (pointer + 2)
                                                        6 -> runProgram program (registers { b = a registers `div` 2 ^ combo }) $ pointer + 2
                                                        7 -> runProgram program (registers { c = a registers `div` 2 ^ combo }) $ pointer + 2
                                                        _ -> error "Invalid opcode."
    where literal = program !! (pointer + 1)
          combo = case literal of
                       4 -> a registers
                       5 -> b registers
                       6 -> c registers
                       x -> x

findMinA :: Program -> Registers -> Int -> [Int] -> Int
findMinA program registers i oct | output == program = a
                                 | output == drop (length program - length oct - 1) program = findMinA program registers 0 (i:oct)
                                 | otherwise = findMinA program registers (i + 1) oct
    where idx = [0..] :: [Int]
          a = sum $ zipWith (\d n -> d * 8 ^ n) (i:oct) idx
          output = runProgram program (registers { a = a }) 0

part1 :: Solution
part1 input = let (registers, program) = parseInput input
              in Msg $ intercalate "," $ map show $ runProgram program registers 0

part2 :: Solution
part2 input = let (registers, program) = parseInput input
              in V $ findMinA program registers 0 []
