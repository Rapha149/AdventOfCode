module Years.Year2023.Day15 (part1, part2) where

import Util.Util
import Data.Char
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

hash :: String -> Int
hash = foldl (\v c -> (v + ord c) * 17 `mod` 256) 0

perform :: Map Int [(String, Int)] -> String -> Map Int [(String, Int)]
perform boxes operation = case break (`elem` "-=") operation of
                               (label, "-") -> Map.adjust (filter ((/= label) . fst)) (hash label) boxes
                               (label, ['=',n]) -> Map.adjust (insert label $ digitToInt n) (hash label) boxes
                               _ -> error "Invalid operation."
    where insert :: String -> Int -> [(String, Int)] -> [(String, Int)]
          insert label n xs = case break ((== label) . fst) xs of
                                 (before, _:after) -> before ++ ((label, n) : after)
                                 _ -> xs ++ [(label, n)]

part1 :: Solution
part1 = V . sum . map hash . splitOn "," . hd

part2 :: Solution
part2 = V . Map.foldrWithKey (\k v -> (+) ((k + 1) * sum (zipWith (*) (map snd v) [1..]))) 0 . foldl perform (Map.fromList $ map (,[]) [0..255]) . splitOn "," . hd
