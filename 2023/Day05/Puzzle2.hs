module Main where
import System.Environment (getArgs)
import Data.List.Split

type Mappings = [(Int, Int, Int)]

parseSeedRanges :: [Int] -> [(Int, Int)]
parseSeedRanges [] = []
parseSeedRanges (a:b:xs) = (a, a + b) : parseSeedRanges xs

intersection :: Int -> Int -> Int -> Int -> (Bool, Int, Int)
intersection a b c d | e < f = (True, e, f)
                     | otherwise = (False, 0, 0)
    where e = max a c
          f = min b d

mapSeedRanges :: [(Int, Int)] -> Mappings -> [(Int, Int)]
mapSeedRanges [] _ = []
mapSeedRanges ((x,y):xs) mappings = checkMappings mappings
    where checkMappings :: Mappings -> [(Int, Int)]
          checkMappings [] = (x,y) : mapSeedRanges xs mappings
          checkMappings ((d,a,b):ms) = let (exists, ix, iy) = intersection x y a b
                                       in if exists then handleMapping ix iy (d - a) else checkMappings ms
          handleMapping :: Int -> Int -> Int -> [(Int, Int)]
          handleMapping ix iy d | x < ix && iy < y = (ix + d, iy + d) : mapSeedRanges ((x,ix):(iy,y):xs) mappings
                                | x < ix = (ix + d, y + d) : mapSeedRanges ((x,ix):xs) mappings
                                | iy < y = (x + d, iy + d) : mapSeedRanges ((iy,y):xs) mappings
                                | otherwise = (x + d, y + d) : mapSeedRanges xs mappings

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    let categories = map lines $ splitOn "\n\n" content
        seeds = parseSeedRanges $ map read $ words $ drop 7 $ categories !! 0 !! 0 :: [(Int, Int)]
        mappings = map (\(l:ls) -> map (\[d,s,l] -> (d, s, s + l)) $ map (map read . words) ls) $ drop 1 categories :: [Mappings]
    print $ minimum $ map fst $ foldl' mapSeedRanges seeds mappings
