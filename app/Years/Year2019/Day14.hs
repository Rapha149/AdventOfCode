module Years.Year2019.Day14 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Reactions = Map String (Int, [(String, Int)])

parseInput :: [String] -> Reactions
parseInput [] = Map.empty
parseInput (l:ls) = Map.insert chemical (amount, map parseChemical $ splitOn ", " inputs) $ parseInput ls
    where (inputs, output) = pair $ splitOn " => " l
          (chemical, amount) = parseChemical output
          parseChemical :: String -> (String, Int)
          parseChemical = second read . swap . pair . words

topoSort :: Reactions -> [String] -> Int -> (Map String Int, Int)
topoSort _ [] num = (Map.empty, num)
topoSort graph (node:xs) num | Map.notMember node graph = topoSort graph xs num
                             | otherwise = first (Map.union hierarchy') $ topoSort (Map.difference graph' hierarchy) xs $ num' - 1
    where graph' = Map.delete node graph
          (hierarchy, num') = topoSort graph' (map fst $ snd $ graph Map.! node) num
          hierarchy' = Map.insert node num' hierarchy

getHierarchy :: Reactions -> [String]
getHierarchy reactions = map fst $ sortOn snd $ Map.toList $ fst $ topoSort reactions ["FUEL"] 0

getFuelCost :: Reactions -> [String] -> Map String Int -> Int
getFuelCost _ [] counts = counts Map.! "ORE"
getFuelCost reactions (chem:xs) counts = getFuelCost reactions xs $ Map.unionWith (+) counts counts'
    where (amount, inputs) = reactions Map.! chem
          recipeCount = counts Map.! chem `divCeil` amount
          counts' = Map.map (* recipeCount) $ Map.fromList inputs

part1 :: Solution
part1 input = V $ getFuelCost reactions (getHierarchy reactions) (Map.singleton "FUEL" 1)
    where reactions = parseInput input


getMaxFuel :: Reactions -> [String] -> Int -> Int
getMaxFuel reactions hierarchy target = binSearch initLow (initLow * 2)
    where initLow = target `div` getFuelCost reactions hierarchy (Map.singleton "FUEL" 1)
          binSearch :: Int -> Int -> Int
          binSearch low high | high - low <= 1 = low
                             | otherwise = let fuel = (low + high) `div` 2
                                               cost = getFuelCost reactions hierarchy (Map.singleton "FUEL" fuel)
                                           in if cost < target then binSearch fuel high else binSearch low fuel

part2 :: Solution
part2 input = V $ getMaxFuel reactions (getHierarchy reactions) 1000000000000
    where reactions = parseInput input
