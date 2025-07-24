module Main where
import System.Environment (getArgs)
import Data.List.Split
import qualified Data.Map.Strict as Map

getCardCounts :: [String] -> Int -> Map.Map Int Int -> Map.Map Int Int
getCardCounts [] _ counts = counts
getCardCounts (card:cs) i counts | correct > 0 = getCardCounts cs (i + 1) $ foldr (\x -> Map.insertWith (+) x count) counts [i+1 .. i+correct]
                                 | otherwise = getCardCounts cs (i + 1) counts
    where numbers = splitOn "|" $ (splitOn ":" $ card) !! 1
          winning = words $ numbers !! 0
          yours = words $ numbers !! 1
          correct = length (filter (`elem` winning) yours)
          count = counts Map.! i


main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    let contentLines = lines content
        initial = Map.fromList $ map (, 1) [1..length contentLines]
        counts = getCardCounts contentLines 1 initial
    print $ Map.foldr (+) 0 counts
