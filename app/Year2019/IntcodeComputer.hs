module Year2019.IntcodeComputer (State (..), parseState, parseStateI, run) where

import Util
import Data.Maybe
import Data.List.Split
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

data Instruction = Instruction { params :: [Bool], output :: Output, fun :: [Int] -> Maybe Int }
data Output = Write | Jump | Output
data State = State { inputs :: [Int], outputs :: [Int], finished :: Bool, ip :: Int, program :: IntMap Int }

parseState :: [String] -> State
parseState = State [] [] False 0 . IM.fromList . zip [0..] . map read . splitOn "," . hd

parseStateI :: [Int] -> [String] -> State
parseStateI inputs = (\state -> state { inputs = inputs }) . parseState

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

run :: State -> State
run state@State {..} | op == 99 = state { finished = True }
                     | isNothing parsed = state
                     | isNothing result = run $ state { inputs = inputs', ip = ip' }
                     | otherwise = let res = fromJust result
                                   in case output of
                                           Write -> run $ state { inputs = inputs', ip = ip' + 1, program = IM.insert (program IM.! ip') res program }
                                           Jump -> run $ state { inputs = inputs', ip = res }
                                           Output -> (\s -> s { outputs = res : s.outputs }) $ run $ state { inputs = inputs', ip = ip' }
    where opRaw = program IM.! ip
          op = opRaw `mod` 100
          Instruction {..} = instruction op
          parsed = parseParameters inputs (ip + 1) 100 [] params
          (inputs', ip', values) = fromJust parsed
          result = fun values
          parseParameters :: [Int] -> Int -> Int -> [Int] -> [Bool]-> Maybe ([Int], Int, [Int])
          parseParameters ins i _ vals [] = Just (ins, i, reverse vals)
          parseParameters ins i mode vals (False:ps) = let val = program IM.! i
                                                           in case (opRaw `div` mode) `mod` 10 of
                                                                   0 -> parseParameters ins (i + 1) (mode * 10) (program IM.! val : vals) ps
                                                                   1 -> parseParameters ins (i + 1) (mode * 10) (val : vals) ps
                                                                   _ -> error "Invalid parameter mode."
          parseParameters [] _ _ _ (True:_) = Nothing
          parseParameters ins i mode vals (True:ps) = parseParameters (tl ins) i mode (hd ins : vals) ps
