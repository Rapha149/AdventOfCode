module Year2021.Day10 (part1, part2) where

import Util
import Data.List.Extra

data Type = Round | Square | Curly | Angle deriving (Eq, Enum)
data ChunkChar = Open Type | Close Type

parseLine :: String -> [ChunkChar]
parseLine = map (\case '(' -> Open Round
                       ')' -> Close Round
                       '[' -> Open Square
                       ']' -> Close Square
                       '{' -> Open Curly
                       '}' -> Close Curly
                       '<' -> Open Angle
                       '>' -> Close Angle
                       _ -> error "Invalid character.")

getErrorScore :: [Type] -> [ChunkChar] -> Int
getErrorScore _ [] = 0
getErrorScore open (Open o:xs) = getErrorScore (o:open) xs
getErrorScore (o:open) (Close c:xs) | o == c = getErrorScore open xs
                                    | otherwise = case c of
                                                       Round -> 3
                                                       Square -> 57
                                                       Curly -> 1197
                                                       Angle -> 25137
getErrorScore [] (Close _:_) = error "Invalid line."

part1 :: Solution
part1 = V . sumOn' (getErrorScore [] . parseLine)


getAutocompleteScore :: [Type] -> [ChunkChar] -> Int
getAutocompleteScore open [] = foldl (\acc t -> acc * 5 + 1 + fromEnum t) 0 open
getAutocompleteScore open (Open o:xs) = getAutocompleteScore (o:open) xs
getAutocompleteScore (o:open) (Close c:xs) | o == c = getAutocompleteScore open xs
                                           | otherwise = 0 -- ignore corrupted
getAutocompleteScore [] (Close _:_) = error "Invalid line."

part2 :: Solution
part2 input = let scores = sort $ filter (> 0) $ map (getAutocompleteScore [] . parseLine) input
              in V $ scores !! (length scores `div` 2)
