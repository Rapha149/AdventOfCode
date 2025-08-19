module Year2023.Day20 (part1, part2) where

import Util
import Data.List.Split
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Module = Broadcaster [String] | FlipFlop Bool [String] | Conjunction (Map String Bool) [String]

getOutputs :: Module -> [String]
getOutputs (Broadcaster out) = out
getOutputs (FlipFlop _ out) = out
getOutputs (Conjunction _ out) = out

parseInput :: [String] -> Map String Module
parseInput input = Map.foldrWithKey (uncurry Map.insert .: toModule) Map.empty outputs
    where outputs = Map.fromList $ map (second (splitOn ", ") . tuple . splitOn " -> ") input
          toModule :: String -> [String] -> (String, Module)
          toModule "broadcaster" out = ("broadcaster", Broadcaster out)
          toModule ('%':name) out = (name, FlipFlop False out)
          toModule ('&':name) out = (name, Conjunction (Map.fromList $ map ((, False) . dropWhile (`elem` "%&")) $ Map.keys $ Map.filter (elem name) outputs) out)
          toModule _ _ = error "Invalid module."

sendPulse :: Map String Module -> [(String, String, Bool)] -> (Map String Module, Map (String, Bool) Int)
sendPulse modules [] = (modules, Map.empty)
sendPulse modules ((source, dest, high):ps) = second (Map.insertWith (+) (source, high) 1) $ sendPulse modules' (ps ++ next)
    where (modules', ps', high') = case modules Map.!? dest of
                                 Nothing -> (modules, [], False)
                                 (Just (Broadcaster out)) -> (modules, out, high)
                                 (Just (FlipFlop on out)) -> if high then (modules, [], False) else (Map.insert dest (FlipFlop (not on) out) modules, out, not on)
                                 (Just (Conjunction inputs out)) -> let newInputs = Map.insert source high inputs
                                                                    in (Map.insert dest (Conjunction newInputs out) modules, out, not $ Map.foldr (&&) True newInputs)
          next = map (dest,, high') ps'

countPulses :: Int -> Map String Module -> (Int, Int)
countPulses 0 _ = (0, 0)
countPulses n modules = onBoth (+) counts $ countPulses (n - 1) modules'
    where (modules', pulses) = sendPulse modules [("button", "broadcaster", False)]
          counts = both (Map.foldr (+) 0) $ Map.partitionWithKey (const . snd) pulses

getVitalConjunctions :: Map String Module -> (String, [String])
getVitalConjunctions modules = (output, Map.keys $ Map.filter (elem output . getOutputs) modules)
    where output = hd $ Map.keys $ Map.filter (elem "rx" . getOutputs) modules

findLoops :: Map String Module -> String -> [String] -> Int -> Map String (Int, Bool) -> [Int]
findLoops _ _ [] _ loops = map fst $ Map.elems loops
findLoops modules output cons n loops = let loops' = foldr testLoop loops filtered
                                        in findLoops modules' output (filter (maybe True snd . (Map.!?) loops') cons) (n + 1) loops'
    where (modules', pulses) = sendPulse modules [("button", "broadcaster", False)]
          filtered = map fst $ filter (\(s, h) -> s `elem` cons && h) $ Map.keys pulses
          testLoop :: String -> Map String (Int, Bool) -> Map String (Int, Bool)
          testLoop name loops' | Map.notMember name loops' = Map.insert name (n, True) loops'
                               | otherwise = let (loop, _) = loops' Map.! name
                                             in if n == loop * 2 then Map.insert name (loop, False) loops'
                                                                 else error $ "Loop did not match for " <> name

part1 :: Solution
part1 = V . uncurry (*) . countPulses 1000 . parseInput

part2 :: Solution
part2 input = let modules = parseInput input
                  (output, cons) = getVitalConjunctions modules
              in V $ foldr lcm 1 $ findLoops modules output cons 1 Map.empty
