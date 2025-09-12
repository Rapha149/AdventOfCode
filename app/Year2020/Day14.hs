module Year2020.Day14 (part1, part2) where

import Util
import Data.Bits
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Mask = (Int, Int, [Int])
data Action = SetMask Mask | Write Int Int

parseInput :: [String] -> [Action]
parseInput = map (\l -> case words l of
                             ["mask", "=", mask] -> let chars = Map.fromListWith (++) $ [('0', []), ('1', []), ('X', [])] ++ [(x, [i]) | (i, x) <- zip [35, 34 .. 0] mask]
                                                    in SetMask (foldl clearBit oneBits $ chars Map.! '0',
                                                                foldl setBit zeroBits $ chars Map.! '1',
                                                                chars Map.! 'X')
                             [address, "=", value] -> Write (read $ ini $ drop 4 address) (read value)
                             _ -> error "Invalid action.")

run1 :: Mask -> [Action] -> Map Int Int
run1 _ [] = Map.empty
run1 _ (SetMask mask : as) = run1 mask as
run1 mask@(zeros, ones, _) (Write addr val : as) = Map.insertWith seq addr (val .&. zeros .|. ones) $ run1 mask as

part1 :: Solution
part1 = V . sum . run1 (oneBits, zeroBits, []) . parseInput


run2 :: Mask -> [Action] -> Map Int Int
run2 _ [] = Map.empty
run2 _ (SetMask mask : as) = run2 mask as
run2 mask@(_, ones, xs) (Write addr val : as) = Map.union (run2 mask as) memory
    where addr' = foldl clearBit (addr .|. ones) xs
          memory = Map.fromList $ map ((, val) . foldl setBit addr') $ subsequences xs

part2 :: Solution
part2 = V . sum . run2 (oneBits, zeroBits, []) . parseInput
