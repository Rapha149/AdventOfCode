module Main where
import Text.Regex.TDFA
import Data.Maybe
import Numeric.LinearAlgebra

parseLine :: String -> (Int, Int)
parseLine line = (read $ groups !! 1, read $ groups !! 2)
    where (_, _, _, groups) = line =~ "^(Button [AB]|Prize): X[+=]([0-9]+), Y[+=]([0-9]+)$" :: (String, String, String, [String])

parseContent :: [String] -> [(Matrix Double, Matrix Double)]
parseContent [] = []
parseContent ([]:ls) = parseContent ls
parseContent (a:b:p:ls) = ((2><2) $ map fromIntegral [a1, b1, a2, b2], (2><1) $ map (\x -> fromIntegral $ x + 10000000000000) [p1, p2]) : parseContent ls
    where (a1, a2) = parseLine a
          (b1, b2) = parseLine b
          (p1, p2) = parseLine p

getTokens :: (Matrix Double, Matrix Double) -> Maybe Int
getTokens (left, right) | result == Nothing = Nothing
                        | otherwise = let [[a], [b]] = toLists $ fromJust result
                                          aInt = round a
                                          bInt = round b
                                      in if (abs $ a - fromIntegral aInt) < 0.0001 && (abs $ b - fromIntegral bInt) < 0.0001 then Just $ aInt * 3 + bInt else Nothing
    where result = linearSolve left right

main :: IO ()
main = do
    content <- readFile "input.txt"
    let equations = parseContent $ lines content
    print $ sum $ map (\result -> if result == Nothing then 0 else fromJust result) $ map (\equation -> getTokens equation) equations
