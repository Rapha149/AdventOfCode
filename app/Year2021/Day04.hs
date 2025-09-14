module Year2021.Day04 (part1, part2) where

import Util
import Data.Bits
import Data.Word
import Data.List.Extra
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

data Board = Board { positions :: IntMap Int, marked :: Word32 } deriving Show

parseInput :: [String] -> ([Int], [Board])
parseInput input = (numbers, boards)
    where numbers = map read $ splitOn "," $ hd input
          boards = map parseBoard $ split null $ drop 2 input
          parseBoard :: [String] -> Board
          parseBoard board = let positions = IM.fromList [(read x, r * 5 + c) | (r, row) <- zip [0..] board, (c, x) <- zip [0..] $ words row]
                             in Board { positions, marked = 0 }

winningMasks :: [Word32]
winningMasks = mask 0b11111 5 ++ mask 0b100001000010000100001 1
    where mask x i = map (shiftL x . (*i)) [0..4]

mark :: Int -> Board -> Board
mark n board@Board {..} = case positions IM.!? n of
                               Just i -> board { marked = setBit marked i }
                               Nothing -> board

wins :: Board -> Bool
wins Board {marked} = any (\mask -> (marked .&. mask) == mask) winningMasks

getFirstWinner :: [Int] -> [Board] -> (Int, Board)
getFirstWinner [] _ = error "No winner."
getFirstWinner (n:xs) boards = case find wins boards' of
                                    Just board -> (n, board)
                                    Nothing -> getFirstWinner xs boards'
    where boards' = map (mark n) boards

getLastWinner :: [Int] -> [Board] -> (Int, Board)
getLastWinner [] _ = error "No winner."
getLastWinner _ [] = error "No unique last winner."
getLastWinner (n:xs) boards = case boards' of
                                   [board] -> if wins board then (n, board) else getLastWinner xs boards'
                                   _ -> getLastWinner xs $ filter (not . wins) boards'
    where boards' = map (mark n) boards

getFinalScore :: (Int, Board) -> Int
getFinalScore (number, Board {..}) = number * sum [n | (n, i) <- IM.toList positions, not $ testBit marked i]

part1 :: Solution
part1 = V . getFinalScore . uncurry getFirstWinner . parseInput

part2 :: Solution
part2 = V . getFinalScore . uncurry getLastWinner . parseInput
