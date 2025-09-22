module Years.Year2024.Day21 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra
import Numeric
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Keypad = Map (Char, Char) [String]

getPaths :: Set Vec -> Int -> Vec -> [(Int, Vec, String)] -> [String]
getPaths _ _ _ [] = []
getPaths points maxCost end ((cost, pos, path):xs) | cost > maxCost = []
                                                   | pos == end = reverse ('A':path) : getPaths points maxCost end xs
                                                   | otherwise = getPaths points maxCost end (xs ++ next)
    where next = map (\(p, c) -> (cost + 1, p, c:path)) $ filter ((`Set.member` points) . fst) $
                 map (first (onBoth (+) pos)) [((0, 1), '^'), ((0, -1), 'v'), ((-1, 0), '<'), ((1, 0), '>')]

toKeypad :: [(Char, Vec)] -> Keypad
toKeypad buttons = Map.fromList [((c1, c2), toPaths p1 p2) | (c1, p1) <- buttons, (c2, p2) <- buttons]
    where points = Set.fromList $ map snd buttons
          toPaths p1 p2 = getPaths points (uncurry (+) $ onBoth (abs .: (-)) p1 p2) p2 [(0, p1, "")]

numeric, directional :: Keypad
numeric = toKeypad [('7', (0, 3)), ('8', (1, 3)), ('9', (2, 3)),
                    ('4', (0, 2)), ('5', (1, 2)), ('6', (2, 2)),
                    ('1', (0, 1)), ('2', (1, 1)), ('3', (2, 1)),
                                   ('0', (1, 0)), ('A', (2, 0))]
directional = toKeypad [               ('^', (1, 2)), ('A', (2, 2)),
                        ('<', (0, 1)), ('v', (1, 1)), ('>', (2, 1))]

minCosts :: [Keypad] -> Map (Char, Char) Int -> Map (Char, Char) Int
minCosts [] costs = costs
minCosts (keypad:ks) costs = minCosts ks newCosts
    where newCosts = Map.map (minimum . map (\path -> sumOn' (costs Map.!) $ zip ('A':path) path)) keypad

getComplexity :: Map (Char, Char) Int -> String -> Int
getComplexity costs code = sumOn' (costs Map.!) (zip ('A':code) code) * fst (hd $ readDec code)

part1 :: Solution
part1 input = let costs = minCosts [directional, directional, numeric] (Map.map (const 1) directional)
              in V $ sumOn' (getComplexity costs) input

part2 :: Solution
part2 input = let costs = minCosts (replicate 25 directional ++ [numeric]) (Map.map (const 1) directional)
              in V $ sumOn' (getComplexity costs) input
