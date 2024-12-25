module Main where
import Data.List
import Data.List.Split
import qualified Data.Map as Map

countPossibleCombinations :: [String] -> String -> Int
countPossibleCombinations patterns design = matchPatterns (length design - 1) patterns True Map.empty
    where matchPatterns _ [] _ _ = 0
          matchPatterns n (p:ps) first cache
              | n >= length design = 1
              | Map.member n cache = cache Map.! n
              | otherwise = let count = (if isPrefixOf p (drop n design) then matchPatterns (n + length p) patterns True cache else 0) + matchPatterns n ps False cache
                            in if n > 0 && first then matchPatterns (n - 1) patterns True (Map.insert n count cache) else count

main :: IO ()
main = do
    content <- readFile "input.txt"
    let contentLines = lines content
        patterns = splitOn ", " $ head contentLines
        designs = drop 2 contentLines
    print $ sum $ map (countPossibleCombinations patterns) designs
