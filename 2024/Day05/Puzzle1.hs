module Main where
import Data.List.Split

parseRules :: [String] -> [(Int,Int)]
parseRules [] = []
parseRules (r:rs) = (read (head split), read (last split)) : parseRules rs
    where split = splitOn "|" r

parseUpdates :: [String] -> [[Int]]
parseUpdates [] = []
parseUpdates (r:rs) = map read split : parseUpdates rs
    where split = splitOn "," r

doRulesMatch :: [(Int, Int)] -> [Int] -> Int -> Bool
doRulesMatch [] _ _ = True
doRulesMatch (r:rs) ps n = (fst r /= n || not (elem (snd r) ps)) && doRulesMatch rs ps n

isOrdered :: [(Int, Int)] -> [Int] -> [Int] -> Bool
isOrdered _ _ [] = True
isOrdered rs [] (n:ns) = isOrdered rs [n] ns
isOrdered rs ps (n:ns) = doRulesMatch rs ps n && isOrdered rs (n : ps) ns

sumMiddleNumbers :: [[Int]] -> Int
sumMiddleNumbers [] = 0
sumMiddleNumbers (u:us) = u !! (div (length u) 2) + sumMiddleNumbers us

main :: IO ()
main = do
    rulesContent <- readFile "input-rules.txt"
    let rulesLines = lines rulesContent
        rules = parseRules rulesLines

    updatesContent <- readFile "input-updates.txt"
    let updatesLines = lines updatesContent
        updates = parseUpdates updatesLines
        ordered = filter (isOrdered rules []) updates
    print $ sumMiddleNumbers ordered
