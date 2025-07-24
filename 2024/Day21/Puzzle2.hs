module Main where
import qualified Data.Map as Map

buttonToPos :: Bool -> Char -> (Int, Int)
buttonToPos True button = case button of
                            '7' -> (0, 0)
                            '8' -> (1, 0)
                            '9' -> (2, 0)
                            '4' -> (0, 1)
                            '5' -> (1, 1)
                            '6' -> (2, 1)
                            '1' -> (0, 2)
                            '2' -> (1, 2)
                            '3' -> (2, 2)
                            '0' -> (1, 3)
                            'A' -> (2, 3)
buttonToPos False button = case button of
                            '^' -> (1, 0)
                            'A' -> (2, 0)
                            '<' -> (0, 1)
                            'v' -> (1, 1)
                            '>' -> (2, 1)

combos :: [a] -> [(a, a)]
combos xs = [(a, b) | a <- xs, b <- xs]

getPossiblePaths :: Bool -> (Char, Char) -> [String]
getPossiblePaths numpad (a, b) | buttons == [] = ["A"]
                               | length buttons == 1 = [foldr (\(c, n) s -> replicate n c ++ s) "" buttons ++ "A"]
                               | otherwise = map (\xs -> foldr (\(c, n) s -> replicate n c ++ s) "" xs ++ "A") $ filter isAllowed [buttons, reverse buttons]
    where (x1, y1) = buttonToPos numpad a
          (x2, y2) = buttonToPos numpad b
          xDiff = x2 - x1
          yDiff = y2 - y1
          buttons = filter (\(c, n) -> n > 0) [(if xDiff >= 0 then '>' else '<', abs xDiff), (if yDiff >= 0 then 'v' else '^', abs yDiff)]
          isAllowed :: [(Char, Int)] -> Bool
          isAllowed ((c, n):_) = case c of
                                        '^' -> (x1, y1 - n)
                                        'v' -> (x1, y1 + n)
                                        '<' -> (x1 - n, y1)
                                        '>' -> (x1 + n, y1)
                                 /= if numpad then (0, 3) else (0, 0)

getPathsForCombos :: Bool -> [Char] -> [((Char, Char), [String])]
getPathsForCombos numpad = map (\a -> (a, getPossiblePaths numpad a)) . combos

pathToPairs :: String -> [(Char, Char)]
pathToPairs path = pathToPairs' 'A' path
    where pathToPairs' :: Char -> String -> [(Char, Char)]
          pathToPairs' _ [] = []
          pathToPairs' a (b:cs) = (a, b) : pathToPairs' b cs

modifyCosts :: Int -> [((Char, Char), [String])] -> Map.Map (Char, Char) Int -> Map.Map (Char, Char) Int
modifyCosts 0 _ costs = costs
modifyCosts n paths costs = modifyCosts (n - 1) paths $ modifyCosts' paths
    where modifyCosts' :: [((Char, Char), [String])] -> Map.Map (Char, Char) Int
          modifyCosts' [] = Map.empty
          modifyCosts' ((c,ps):otherPaths) = Map.insert c (minimum $ map (\path -> foldr (\a b -> costs Map.! a + b) 0 $ pathToPairs path) ps) $ modifyCosts' otherPaths

getComplexity :: Map.Map (Char, Char) Int -> String -> Int
getComplexity costs code = (sum $ map (costs Map.!) $ pathToPairs code) * read (take (length code - 1) code)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let codes = lines content
        dirPaths = getPathsForCombos False "A<^v>"
        dirCosts = Map.fromList $ map (\(c, p) -> (c, minimum $ map length p)) dirPaths
        newDirCosts = modifyCosts 24 dirPaths dirCosts
        numPaths = getPathsForCombos True "A0123456789"
        numCosts = modifyCosts 1 numPaths newDirCosts
    print $ sum $ map (getComplexity numCosts) codes
