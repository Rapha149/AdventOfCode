module Year2024.Day24 (part1, part2) where

import Util
import Data.Bits
import Data.List
import Data.List.Split
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf

data Gate = G { in1 :: String, in2 :: String, op :: Int -> Int -> Int }

parseInput :: [String] -> (Map String Int, Map String Gate)
parseInput input = (Map.fromList $ map (second read . tuple . splitOn ": ") wireInput,
                    Map.fromList $ map (parseGate . words) gateInput)
    where (wireInput, gateInput) = tuple $ splitOn [""] input
          parseGate :: [String] -> (String, Gate)
          parseGate [in1, op, in2, _, out] = (out, G { op = case op of
                                                                 "AND" -> (.&.)
                                                                 "OR" -> (.|.)
                                                                 "XOR" -> xor
                                                                 _ -> error "Unknown operation.", .. })
          parseGate _ = error "Invalid gate."

getResult :: Map String Int -> Map String Gate -> String -> Int
getResult wires gates gate | Map.member gate wires = wires Map.! gate
                           | otherwise = op (getResult wires gates in1) (getResult wires gates in2)
      where G {..} = gates Map.! gate

validate :: Int -> Map String Gate -> Int -> Bool
validate bits gates n = all validate' [(x, y, c) | x <- if n < bits then [0, 1] else [0], y <- if n < bits then [0, 1] else [0], c <- if n > 0 then [0, 1] else [0]]
      where zeros = map (, 0) $ [0 .. n - 2] ++ [n + 1 .. bits]
            makeWires :: Char -> Int -> Int -> [(String, Int)]
            makeWires char x c = map (first (printf "%c%02d" char)) $ (n - 1, c) : (n, x) : zeros
            validate' :: (Int, Int, Int) -> Bool
            validate' (x, y, c) = let wires = Map.fromList $ makeWires 'x' x c ++ makeWires 'y' y c
                                  in getResult wires gates (printf "z%02d" n) == (x + y + c) `mod` 2

getImpactWires :: Map String Gate -> String -> [String]
getImpactWires gates gate | Map.notMember gate gates = []
                          | otherwise = gate : getImpactWires gates in1 ++ getImpactWires gates in2
      where G {..} = gates Map.! gate

fix :: Int -> Map String Gate -> [String] -> Int -> [String]
fix bits gates wires n | n > bits = []
                       | validate bits gates n = fix bits gates (wires \\ impact) $ n + 1
                       | otherwise = case fixed of
                                          (((a, b), gates'):_) ->  a : b : fix bits gates' (wires \\ impact) (n + 1)
                                          [] -> error $ printf "Could not fix gate %d" n
      where swapGates :: (String, String) -> Map String Gate
            swapGates (a, b) = Map.insert a (gates Map.! b) $ Map.insert b (gates Map.! a) gates
            isLoopFree :: Map String Gate -> Set String -> [String] -> Bool
            isLoopFree _ _ [] = True
            isLoopFree gates' found (g:gs) | Set.member g found = False
                                           | Map.notMember g gates' = isLoopFree gates' found gs
                                           | otherwise = let G {..} = gates' Map.! g
                                                         in isLoopFree gates' (Set.insert g found) (in1 : in2 : gs)
            z = printf "z%02d" n
            impact = intersect wires $ getImpactWires gates z
            fixed = [((a, b), gates') | a <- impact, b <- wires, a /= b, let gates' = swapGates (a, b),
                                        isLoopFree gates' Set.empty [z], validate bits gates' n]

part1 :: Solution
part1 input = let (wires, gates) = parseInput input
                  zValues = map (getResult wires gates) $ filter ((== 'z') . hd) $ Map.keys gates
              in V $ foldr (\i acc -> i + acc * 2) 0 zValues

part2 :: Solution
part2 input = let (_, gates) = parseInput input
                  bits = read (tl $ lst $ filter ((== 'z') . hd) $ Map.keys gates)
              in Msg $ intercalate "," $ sort $ fix bits gates (Map.keys gates) 0
