module Years.Year2023.Day08 (part1, part2) where

import Util.Util
import Data.List
import Data.Maybe
import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Network = Map String (String, String)

parseLine :: [String] -> (String, (String, String))
parseLine [s, _, l, r] = (s, (take 3 $ drop 1 l, take 3 r))
parseLine _ = error "Invalid number of words."

countSteps :: Network -> String -> String -> String -> Int
countSteps _ _ _ "ZZZ" = 0
countSteps network instructions [] node = countSteps network instructions instructions node
countSteps network instructions (i:is) node = 1 + countSteps network instructions is ((if i == 'L' then fst else snd) $ network Map.! node)

findLoop :: Network -> String -> String -> Int -> String -> [(String, Int)] -> (Int, Int)
findLoop network instructions [] _ node path = findLoop network instructions instructions 0 node path
findLoop network instructions (i:is) x node path | (node, x) `elem` path = let revPath = reverse path
                                                                               idx = fromJust $ elemIndex (node, x) revPath
                                                                               loop = map fst $ drop idx revPath
                                                                               first = stepsUntilEnd loop
                                                                           in (idx + first, repeatsAfter loop $ first + 1)
                                                 | otherwise = findLoop network instructions is (x + 1) ((if i == 'L' then fst else snd) $ network Map.! node) ((node, x):path)
    where stepsUntilEnd :: [String] -> Int
          stepsUntilEnd ((_:_:"Z"):_) = 0
          stepsUntilEnd (_:ns) = 1 + stepsUntilEnd ns
          stepsUntilEnd _ = error "End not found."
          repeatsAfter :: [String] -> Int -> Int
          repeatsAfter loop idx | loop !! (idx `mod` length loop) !! 2 == 'Z' = 1
                                | otherwise = 1 + repeatsAfter loop (idx + 1)

loopToString :: Network -> String -> String -> String
loopToString network instructions s = let (first, repeats) = findLoop network instructions [] 0 s []
                                      in printf "[%s] first occurence: %d, repeats after: %d" s first repeats

firstOccurrence :: Network -> String -> String -> String -> Int
firstOccurrence _ _ _ (_:_:"Z") = 0
firstOccurrence network instructions [] node = firstOccurrence network instructions instructions node
firstOccurrence network instructions (i:is) node = 1 + firstOccurrence network instructions is ((if i == 'L' then fst else snd) $ network Map.! node)

part1 :: Solution
part1 input = let instructions = hd input
                  network = Map.fromList $ map (parseLine . words) $ drop 2 input
              in V $ countSteps network instructions [] "AAA"

part2 :: Solution
part2 raw = let (loops, input) = getExtra1 (== "loops") (const True) False raw
                instructions = hd input
                network = Map.fromList $ map (parseLine . words) $ drop 2 input
                startNodes = filter ((== 'A') . (!! 2)) $ Map.keys network
            in if loops then Msg $ intercalate "\n" $ map (loopToString network instructions) startNodes
                        else V $ foldr (lcm . firstOccurrence network instructions []) 1 startNodes
