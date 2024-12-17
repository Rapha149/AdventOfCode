module Main where
import Data.Char
import Data.List
import Data.List.Split
import Data.Bits
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Regex.TDFA
import Data.Maybe
import Debug.Trace

data Registers = Registers { a :: Int, b :: Int, c :: Int } deriving Show

parseRegisterValue :: String -> Int
parseRegisterValue line = read $ (words line) !! 2

runProgram :: Map.Map Int Int -> Registers -> Int -> [Int]
runProgram program registers pointer | Map.notMember pointer program = []
                                     | otherwise = let comboOperand = case operand of
                                                            0 -> 0
                                                            1 -> 1
                                                            2 -> 2
                                                            3 -> 3
                                                            4 -> a registers
                                                            5 -> b registers
                                                            6 -> c registers
                                                   in case opcode of
                                                           0 -> runProgram program (registers { a = a registers `div` 2 ^ comboOperand }) $ pointer + 2
                                                           1 -> runProgram program (registers { b = b registers `xor` operand }) $ pointer + 2
                                                           2 -> runProgram program (registers { b = comboOperand `mod` 8 }) $ pointer + 2
                                                           3 -> runProgram program registers $ if a registers /= 0 then operand else pointer + 2
                                                           4 -> runProgram program (registers { b = b registers `xor` c registers }) $ pointer + 2
                                                           5 -> (comboOperand `mod` 8) : (runProgram program registers $ pointer + 2)
                                                           6 -> runProgram program (registers { b = a registers `div` 2 ^ comboOperand }) $ pointer + 2
                                                           7 -> runProgram program (registers { c = a registers `div` 2 ^ comboOperand }) $ pointer + 2
    where Just opcode = Map.lookup pointer program
      Just operand = Map.lookup (pointer + 1) program


main :: IO ()
main = do
    content <- readFile "input.txt"
    let contentLines = lines content
        registers = Registers { a = parseRegisterValue $ contentLines !! 0, b = parseRegisterValue $ contentLines !! 1, c = parseRegisterValue $ contentLines !! 2 }
        program = Map.fromList $ zip [0..] $ map read $ splitOn "," $ words (contentLines !! 4) !! 1 :: Map.Map Int Int
    print $ runProgram program registers 0
