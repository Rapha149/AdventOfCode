module Years.Year2019.Day23 (part1, part2) where

import Util.Util
import Years.Year2019.IntcodeComputer
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Network = Map Int (State, [[Int]])

runComputer :: Network -> Int -> (Network, [Int])
runComputer computers n | state'.status == WaitForInput = (Map.insert n (state' { inputs = drop 1 inputs }, queue') computers, [])
                        | dest == 255 = (computers', [x, y])
                        | otherwise = (Map.adjust (second (++ [[x, y]])) dest computers', [])
    where (state, queue) = computers Map.! n
          (inputs, queue') = fromMaybe ([-1], []) $ uncons queue
          state' = runUntilIO $ state { inputs = state.inputs ++ take 1 inputs }
          state'' = runUntilIO $ runUntilIO state'
          (dest, x, y) = triple state''.outputs
          computers' = Map.insert n (state'' { inputs = [], outputs = [] }, queue) computers

runComputers1 :: Network -> Int -> Int
runComputers1 computers n | Map.notMember n computers = runComputers1 computers 0
                          | null to255 = runComputers1 computers' $ n + 1
                          | otherwise = lst to255
    where (computers', to255) = runComputer computers n

part1 :: Solution
part1 input = V $ runComputers1 (Map.fromList [(n, (state { inputs = [n] }, [])) | n <- [0..49]]) 0
    where state = parseState input


runComputers2 :: Network -> Int -> [Int] -> [Int] -> Int
runComputers2 computers n natLast natYs
        | Map.notMember n computers = runComputers2 computers 0 natLast natYs
        | not $ all (\(s, q) -> null s.inputs && null q) computers = runComputers2 computers' (n + 1) (if null to255 then natLast else to255) natYs
        | natY `elem` natYs = natY
        | otherwise = runComputers2 (Map.adjust (second (const [natLast])) 0 computers) 0 natLast (natY:natYs)
    where natY = lst natLast
          (computers', to255) = runComputer computers n

part2 :: Solution
part2 input = V $ runComputers2 (Map.fromList [(n, (state { inputs = [n] }, [])) | n <- [0..49]]) 0 [] []
    where state = parseState input
