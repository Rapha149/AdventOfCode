module Main where
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Bits
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

getGroups :: Set.Set (String, String) -> [String] -> [(String, String, String)]
getGroups connections [] = []
getGroups connections (c:cs) = map (\[a, b] -> (a, b, c)) combinations ++ getGroups connections cs
    where (part, _) = partition (\a -> Set.member (a, c) connections) cs
          combinations = filter (\[a, b] -> Set.member (a, b) connections) $ filter ((2 ==) . length) $ subsequences part

countGroupsWithT :: [(String, String, String)] -> Int
countGroupsWithT [] = 0
countGroupsWithT ((a,b,c):gs) = fromEnum (matches a || matches b || matches c) + countGroupsWithT gs
    where matches :: String -> Bool
          matches (x:xs) = x == 't'

main :: IO ()
main = do
    content <- readFile "input.txt"
    let connections = foldr (\[a, b] -> Set.insert (a, b) . Set.insert (b, a)) Set.empty $ map (splitOn "-") $ lines content
        computers = Set.toList $ Set.foldr (\a -> Set.insert (fst a) . Set.insert (snd a)) Set.empty connections
        groups = getGroups connections computers
    print $ countGroupsWithT groups
