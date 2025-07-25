module Main where
import System.Environment (getArgs)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

countSteps :: Map String (String, String) -> String -> String -> String -> Int
countSteps _ _ _ "ZZZ" = 0
countSteps network instructions [] node = countSteps network instructions instructions node
countSteps network instructions (i:is) node = 1 + countSteps network instructions is ((if i == 'L' then fst else snd) $ network Map.! node)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ if null args then "input.txt" else args !! 0
    let contentLines = lines content
        instructions = contentLines !! 0
        network = Map.fromList $ map (\[s, _, l, r] -> (s, (take 3 $ drop 1 l, take 3 r))) $ map words $ drop 2 contentLines
    print $ countSteps network instructions [] "AAA"
