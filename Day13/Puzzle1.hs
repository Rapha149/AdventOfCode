module Main where
import Data.Char
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict
import qualified Data.Set as Set
import Text.Regex.TDFA
import Data.Maybe

parseLine :: String -> (Int, Int)
parseLine line = (read $ groups !! 1, read $ groups !! 2)
    where (_, _, _, groups) = line =~ "^(Button [AB]|Prize): X[+=]([0-9]+), Y[+=]([0-9]+)$" :: (String, String, String, [String])

parseContent :: [String] -> [((Int, Int), (Int, Int), (Int, Int))]
parseContent [] = []
parseContent ([]:ls) = parseContent ls
parseContent (a:b:p:ls) = (parseLine a, parseLine b, parseLine p) : parseContent ls

getMinimalTokens :: ((Int, Int), (Int, Int), (Int, Int)) -> Int -> Int -> Maybe Int
getMinimalTokens _ 101 _ = Nothing
getMinimalTokens equation a 101 = getMinimalTokens equation (a + 1) 0
getMinimalTokens equation a b | r1 == p1 && r2 == p2 = (\current other -> if other == Nothing then current else min current other) (Just $ a * 3 + b) $ getMinimalTokens equation a (b + 1)
                              | r1 > p1 || r2 > p2 = getMinimalTokens equation (a + 1) 0
                              | otherwise = getMinimalTokens equation a (b + 1)
    where ((a1, a2), (b1, b2), (p1, p2)) = equation
          r1 = a * a1 + b * b1
          r2 = a * a2 + b * b2

main :: IO ()
main = do
    content <- readFile "input.txt"
    let equations = parseContent $ lines content
    print $ sum $ map (\result -> if result == Nothing then 0 else fromJust result) $ map (\equation -> getMinimalTokens equation 0 0) equations
