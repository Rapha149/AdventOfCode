module Years.Year2021.Day24 (part1, part2) where

import Util.Util
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

data Registers = Registers { w :: Int, x :: Int, y :: Int, z :: Int }
type InstructionBlock = Int -> Int -> Int

parseInput :: [[String]] -> [InstructionBlock]
parseInput [] = []
parseInput (["inp", ir]:xs) = block : parseInput rest
    where (operators, rest) = span ((/= "inp") . hd) xs
          block :: InstructionBlock
          block lastZ inp = z $ foldl parseOperator (set ir inp $ Registers 0 0 0 lastZ) operators
          parseOperator :: Registers -> [String] -> Registers
          parseOperator reg [op, a, b] = let f = case op of
                                                      "add" -> (+)
                                                      "mul" -> (*)
                                                      "div" -> div
                                                      "mod" -> mod
                                                      "eql" -> fromEnum .: (==)
                                                      _ -> error "Invalid operator function."
                                         in set a (get a reg `f` get b reg) reg
          parseOperator _ _ = error "Invalid operator."
          get :: String -> Registers -> Int
          get "w" = w
          get "x" = x
          get "y" = y
          get "z" = z
          get v = const $ read v
          set :: String -> Int -> Registers -> Registers
          set "w" v state = state { w = v }
          set "x" v state = state { x = v }
          set "y" v state = state { y = v }
          set "z" v state = state { z = v }
          set _ _ _ = error "Invalid register."
parseInput _ = error "Invalid input."

getModelNumber :: (Int -> Int -> Int) -> IntMap Int -> [InstructionBlock] -> Int
getModelNumber _ numbers [] = numbers IM.! 0
getModelNumber keep numbers (f:fs) = getModelNumber keep (IM.fromListWith keep $ concatMap expand $ IM.toList numbers) fs
    where expand :: (Int, Int) -> [(Int, Int)]
          expand (lastZ, inputs) = [(f lastZ i, inputs * 10 + i) | i <- [1..9]]

part1 :: Solution
part1 = V . getModelNumber max (IM.singleton 0 0) . parseInput . map words

part2 :: Solution
part2 = V . getModelNumber min (IM.singleton 0 0) . parseInput . map words
