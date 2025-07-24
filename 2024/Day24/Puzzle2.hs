module Main where
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map

data Gate = Gate { in1 :: String, in2 :: String, op :: String, out :: String } deriving Show

isInXYGate :: Gate -> Bool
isInXYGate gate = in1 gate !! 0 == 'x' || in2 gate !! 0 == 'x' || in1 gate !! 0 == 'y' || in2 gate !! 0 == 'y'
isOutZGate :: Gate -> Bool
isOutZGate = isZWire . out
isZWire :: String -> Bool
isZWire wire = wire !! 0 == 'z'

isInputFor :: String -> String -> [Gate] -> Bool
isInputFor _  _ [] = False
isInputFor wire operation (g:gs) | (in1 g == wire || in2 g == wire) && op g == operation = True
                                 | otherwise = isInputFor wire operation gs

main :: IO ()
main = do
    initContent <- readFile "input-init.txt"
    gatesContent <- readFile "input-gates.txt"
    let wires = Map.fromList $ map ((\[k, v] -> (k, v == "1")) . splitOn ": ") $ lines initContent
        gates = map ((\[in1, op, in2, _, out] -> Gate { in1 = in1, in2 = in2, op = op, out = out }) . words) $ lines gatesContent
        incorrect1 = map out $ filter (\gate -> isOutZGate gate && out gate /= "z45" && op gate /= "XOR") gates
        incorrect2 = map out $ filter (\gate -> not (isOutZGate gate) && not (isInXYGate gate) && op gate == "XOR") gates
        incorrect3 = map out $ filter (\gate -> isInXYGate gate && in1 gate /= "x00" && in2 gate /= "y00" && not (isOutZGate gate) && op gate == "XOR" && not (isInputFor (out gate) "XOR" gates)) gates
        incorrect4 = map out $ filter (\gate -> op gate == "AND" && in1 gate /= "x00" && in2 gate /= "y00" && not (isInputFor (out gate) "OR" gates)) gates
    putStrLn $ intercalate "," $ sort $ Set.toList $ Set.fromList (incorrect1 ++ incorrect2 ++ incorrect3 ++ incorrect4)
