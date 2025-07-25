module Main where
import System.Environment (getArgs)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Printf

type Network = Map String (String, String)

findLoop :: Network -> String -> String -> Int -> String -> [(String, Int)] -> (Int, Int)
findLoop network instructions [] _ node path = findLoop network instructions instructions 0 node path
findLoop network instructions (i:is) x node path | (node, x) `elem` path = let revPath = reverse path
                                                                               Just idx = elemIndex (node, x) revPath
                                                                               loop = map fst $ drop idx revPath
                                                                               first = stepsUntilEnd loop
                                                                           in (idx + first, repeatsAfter loop $ first + 1)
                                                 | otherwise = findLoop network instructions is (x + 1) ((if i == 'L' then fst else snd) $ network Map.! node) ((node, x):path)
    where stepsUntilEnd :: [String] -> Int
          stepsUntilEnd ((_:_:"Z"):_) = 0
          stepsUntilEnd (_:ns) = 1 + stepsUntilEnd ns
          repeatsAfter :: [String] -> Int -> Int
          repeatsAfter loop i | loop !! (i `mod` length loop) !! 2 == 'Z' = 1
                              | otherwise = 1 + repeatsAfter loop (i + 1)

printLoops :: Network -> String -> [String] -> IO ()
printLoops _ _ [] = return ()
printLoops network instructions (a:as) = do
    let (first, repeats) = findLoop network instructions [] 0 a []
    putStrLn $ printf "[%s] first occurence: %d, repeats after: %d" a first repeats
    printLoops network instructions as

firstOccurrence :: Network -> String -> String -> String -> Int
firstOccurrence _ _ _ (_:_:"Z") = 0
firstOccurrence network instructions [] node = firstOccurrence network instructions instructions node
firstOccurrence network instructions (i:is) node = 1 + firstOccurrence network instructions is ((if i == 'L' then fst else snd) $ network Map.! node)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    let contentLines = lines content
        instructions = contentLines !! 0
        network = Map.fromList $ map (\[s, _, l, r] -> (s, (take 3 $ drop 1 l, take 3 r))) $ map words $ drop 2 contentLines
        startNodes = filter ((== 'A') . (!! 2)) $ Map.keys network

    if length args >= 2 && args !! 1 == "loops"
    then printLoops network instructions startNodes
    else print $ foldr lcm 1 $ map (firstOccurrence network instructions []) startNodes
