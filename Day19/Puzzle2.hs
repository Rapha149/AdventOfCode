module Main where
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

countPossibleCombinations :: [String] -> String -> Int
countPossibleCombinations patterns design = matchPatterns (length design - 1) patterns Map.empty
    where matchPatterns 0 (p:ps) cache | isPrefixOf p design = matchPatterns (length p) patterns cache + matchPatterns 0 ps cache
                                       | otherwise = matchPatterns 0 ps cache
          matchPatterns _ [] _ = 0
          matchPatterns n (p:ps) cache | n >= length design = 1
                                       | Map.member n cache = cache Map.! n
                                       | otherwise = let count = (if isPrefixOf p (drop n design) then matchPatterns (n + length p) patterns cache else 0) + matchPatterns n ps cache
                                                     in if (p:ps) == patterns then matchPatterns (n - 1) patterns (Map.insert n count cache) else count

main :: IO ()
main = do
    content <- readFile "input.txt"
    let contentLines = lines content
        patterns = splitOn ", " $ head contentLines
        designs = drop 2 contentLines
    print $ sum $ map (countPossibleCombinations patterns) designs
