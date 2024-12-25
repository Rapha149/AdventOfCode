module Main where
import Data.List
import Data.List.Split

isDesignPossible :: [String] -> String -> Bool
isDesignPossible patterns design = matchPatterns design patterns
    where matchPatterns [] _ = True
          matchPatterns _ [] = False
          matchPatterns design' (p:ps) = (if isPrefixOf p design' then matchPatterns (drop (length p) design') patterns else False) || matchPatterns design' ps

main :: IO ()
main = do
    content <- readFile "input.txt"
    let contentLines = lines content
        patterns = splitOn ", " $ head contentLines
        designs = drop 2 contentLines
    print $ length $ filter (isDesignPossible patterns) designs
