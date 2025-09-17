module Year2019.IntcodeComputer (parse, run, runIO) where

import Util
import Data.Maybe
import Data.List.Split
import Data.Tuple.Extra
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

data Instruction = Instruction { params :: [Bool], output :: Output, fun :: [Int] -> Maybe Int }
data Output = Write | Jump | Output

parse :: [String] -> [Int]
parse = map read . splitOn "," . hd

instruction :: Int -> Instruction
instruction 1 = Instruction { params = [False, False], output = Write, fun = Just . uncurry (+) . pair }
instruction 2 = Instruction { params = [False, False], output = Write, fun = Just . uncurry (*) . pair }
instruction 3 = Instruction { params = [True], output = Write, fun = Just . hd }
instruction 4 = Instruction { params = [False], output = Output, fun = Just . hd }
instruction 5 = Instruction { params = [False, False], output = Jump, fun = \case (0:_) -> Nothing; [_, a] -> Just a; _ -> Nothing }
instruction 6 = Instruction { params = [False, False], output = Jump, fun = \case [0, a] -> Just a; _ -> Nothing }
instruction 7 = Instruction { params = [False, False], output = Write, fun = Just . fromEnum . uncurry (<) . pair }
instruction 8 = Instruction { params = [False, False], output = Write, fun = Just . fromEnum . uncurry (==) . pair }
instruction _ = error "Invalid opcode."

run :: [Int] -> [Int]
run program = IM.elems $ snd $ execute [] 0 $ IM.fromList $ zip [0..] program

runIO :: [Int] -> [Int] -> ([Int], [Int])
runIO inputs program = second IM.elems $ execute inputs 0 $ IM.fromList $ zip [0..] program

execute :: [Int] -> Int -> IntMap Int -> ([Int], IntMap Int)
execute inputs ip program | op == 99 = ([], program)
                          | isNothing result = execute inputs' ip' program
                          | otherwise = let res = fromJust result
                                        in case output of
                                                Write -> execute inputs' (ip' + 1) $ IM.insert (program IM.! ip') res program
                                                Jump -> execute inputs' res program
                                                Output -> first (res :) $ execute inputs' ip' program
    where opRaw = program IM.! ip
          op = opRaw `mod` 100
          Instruction {..} = instruction op
          (inputs', ip', values) = parseParameters inputs (ip + 1) 100 [] params
          result = fun values
          parseParameters :: [Int] -> Int -> Int -> [Int] -> [Bool]-> ([Int], Int, [Int])
          parseParameters ins i _ vals [] = (ins, i, reverse vals)
          parseParameters ins i mode vals (False:ps) = let val = program IM.! i
                                                           in case (opRaw `div` mode) `mod` 10 of
                                                                   0 -> parseParameters ins (i + 1) (mode * 10) (program IM.! val : vals) ps
                                                                   1 -> parseParameters ins (i + 1) (mode * 10) (val : vals) ps
                                                                   _ -> error "Invalid parameter mode."
          parseParameters ins i mode vals (True:ps) = parseParameters (tl ins) i mode (hd ins : vals) ps
