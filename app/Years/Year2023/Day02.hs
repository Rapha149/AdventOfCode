module Years.Year2023.Day02 (part1, part2) where

import Util.Util
import Data.List.Split
import qualified Data.Map.Strict as Map

config :: String -> Int
config "red" = 12
config "green" = 13
config "blue" = 14
config _ = error "Unknown color."

getPossibleGameNumbers :: [String] -> [Int]
getPossibleGameNumbers [] = []
getPossibleGameNumbers (g:gs) = case splitOn ":" g of
                                     [a, b] -> if isGamePossible b then read (drop 5 a) : getPossibleGameNumbers gs else getPossibleGameNumbers gs
                                     _ -> error "Not two elements."
    where isGamePossible :: String -> Bool
          isGamePossible = all ((\xs -> read (hd xs) <= config (xs !! 1)) . words) . splitOneOf ",;"

getGamePower :: String -> Int
getGamePower = Map.foldr (*) 1 . Map.fromListWith max . map ((\xs -> (xs !! 1, read $ hd xs)) . words) . splitOneOf ",;"

part1 :: Solution
part1 = V . sum . getPossibleGameNumbers

part2 :: Solution
part2 = V . sum . map (getGamePower . (!! 1) . splitOn ":")
