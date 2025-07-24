module Main where
import System.Environment (getArgs)
import Data.List.Split

type Mappings = [(Int, Int, Int)]

mapNumber :: [Mappings] -> Int -> Int
mapNumber [] x = x
mapNumber (m:ms) x = mapNumber ms $ mapNumber' m
    where mapNumber' :: Mappings -> Int
          mapNumber' [] = x
          mapNumber' ((d,a,b):xs) | x >= a && x < b = x - a + d
                                  | otherwise = mapNumber' xs

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    let categories = map lines $ splitOn "\n\n" content
        seeds = map read $ words $ drop 7 $ categories !! 0 !! 0 :: [Int]
        mappings = map (\(l:ls) -> map (\[d,s,l] -> (d, s, s + l)) $ map (map read . words) ls) $ drop 1 categories :: [Mappings]
    print $ minimum $ map (mapNumber mappings) seeds
