module Main where
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Bits
import Data.Char
import Data.Function
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

getGroups :: Set.Set (String, String) -> [String] -> [[String]]
getGroups connections [] = []
getGroups connections (c:cs) = map (c:) combinations ++ getGroups connections cs
    where (part, _) = partition (\a -> Set.member (a, c) connections) cs
          combinations = filter allConnected $ subsequences part
          allConnected :: [String] -> Bool
          allConnected [] = True
          allConnected (a:bs) = all (\b -> Set.member (a, b) connections) bs && allConnected bs

main :: IO ()
main = do
    content <- readFile "input.txt"
    let connections = foldr (\[a, b] -> Set.insert (a, b) . Set.insert (b, a)) Set.empty $ map (splitOn "-") $ lines content
        computers = Set.toList $ Set.foldr (\a -> Set.insert (fst a) . Set.insert (snd a)) Set.empty connections
        groups = getGroups connections computers
        maxGroup = maximumBy (compare `on` length) groups
    putStrLn $ intercalate "," maxGroup
