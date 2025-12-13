module Years.Year2019.IntcodeComputer (Status (..), State (..), parseState, parseStateI, run, runUntilIO) where

import Util.Util
import Data.Maybe
import Data.List.Split
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

data Input = Input { relBase :: Int, values :: [Int] }
data Output = Write | Jump | RelBase | Output
data Instruction = Instruction { params :: [Bool], output :: Output, fun :: Input -> Maybe Int }
data Status = Init | WaitForInput | HasOutput | Finished deriving (Show, Eq)
data State = State { inputs :: [Int], outputs :: [Int], status :: Status, ip :: Int, relBase :: Int, program :: IntMap Int } deriving Show

parseState :: [String] -> State
parseState = State [] [] Init 0 0 . IM.fromList . zip [0..] . map read . splitOn "," . hd

parseStateI :: [Int] -> [String] -> State
parseStateI inputs program = (parseState program) { inputs = inputs }

instruction :: Int -> Instruction
instruction 1 = Instruction { params = [False, False], output = Write, fun = (\[a, b] -> Just $ a + b) . values }
instruction 2 = Instruction { params = [False, False], output = Write, fun = (\[a, b] -> Just $ a * b) . values }
instruction 3 = Instruction { params = [True], output = Write, fun = Just . hd . values }
instruction 4 = Instruction { params = [False], output = Output, fun = Just . hd . values }
instruction 5 = Instruction { params = [False, False], output = Jump, fun = (\case (0:_) -> Nothing; [_, a] -> Just a; _ -> Nothing) . values }
instruction 6 = Instruction { params = [False, False], output = Jump, fun = (\case [0, a] -> Just a; _ -> Nothing) . values }
instruction 7 = Instruction { params = [False, False], output = Write, fun = (\[a, b] -> Just $ fromEnum $ a < b) . values }
instruction 8 = Instruction { params = [False, False], output = Write, fun = (\[a, b] -> Just $ fromEnum $ a == b) . values }
instruction 9 = Instruction { params = [False], output = RelBase, fun = \Input {..} -> Just $ relBase + hd values }
instruction _ = error "Invalid opcode."

run :: State -> State
run state | state'.status == HasOutput = run state'
          | otherwise = state'
    where state' = runUntilIO state

runUntilIO :: State -> State
runUntilIO state@State {..} | op == 99 = state { status = Finished }
                            | isNothing parsed = state { status = WaitForInput }
                            | isNothing result = runUntilIO $ state { inputs = inputs', ip = ip' }
                            | otherwise = let res = fromJust result
                                          in case output of
                                                  Write -> let rel = if getMode ip' == 2 then relBase else 0
                                                           in runUntilIO $ state { inputs = inputs', ip = ip' + 1, program = IM.insert (rel + (program IM.! ip')) res program }
                                                  Jump -> runUntilIO $ state { inputs = inputs', ip = res }
                                                  RelBase -> runUntilIO $ state { inputs = inputs', ip = ip', relBase = res }
                                                  Output -> state { inputs = inputs', outputs = outputs ++ [res], status = HasOutput, ip = ip' }
    where opRaw = program IM.! ip
          op = opRaw `mod` 100
          Instruction {..} = instruction op
          parsed = parseParameters inputs (ip + 1) [] params
          (inputs', ip', values) = fromJust parsed
          result = fun $ Input {..}
          getMode :: Int -> Int
          getMode i = (opRaw `div` 10 ^ (i - ip + 1)) `mod` 10
          parseParameters :: [Int] -> Int -> [Int] -> [Bool]-> Maybe ([Int], Int, [Int])
          parseParameters ins i vals [] = Just (ins, i, reverse vals)
          parseParameters ins i vals (False:ps) = let val = program IM.! i
                                                  in case getMode i of
                                                          0 -> parseParameters ins (i + 1) (IM.findWithDefault 0 val program : vals) ps
                                                          1 -> parseParameters ins (i + 1) (val : vals) ps
                                                          2 -> parseParameters ins (i + 1) (IM.findWithDefault 0 (relBase + val) program : vals) ps
                                                          _ -> error "Invalid parameter mode."
          parseParameters [] _ _ (True:_) = Nothing
          parseParameters ins i vals (True:ps) = parseParameters (tl ins) i (hd ins : vals) ps
