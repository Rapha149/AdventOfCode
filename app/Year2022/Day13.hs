module Year2022.Day13 (part1, part2) where

import Util
import Data.List.Extra

data Element = List [Element] | Num Int deriving Eq

parse :: String -> Element
parse ('[':str) = List $ map parse $ splitTopLevel 0 "" [] $ ini str
    where splitTopLevel :: Int -> String -> [String] -> String -> [String]
          splitTopLevel _ acc parts "" = parts ++ [reverse acc | not $ null acc]
          splitTopLevel depth acc parts (c:cs) | c == '[' = splitTopLevel (depth + 1) (c:acc) parts cs
                                               | c == ']' = splitTopLevel (depth - 1) (c:acc) parts cs
                                               | c == ',' && depth == 0 = splitTopLevel 0 "" (parts ++ [reverse acc]) cs
                                               | otherwise = splitTopLevel depth (c:acc) parts cs
parse n = Num $ read n

cmp :: Element -> Element -> Ordering
cmp (List xs) (List ys) = cmpLists xs ys
cmp x@(List _) y = cmp x (List [y])
cmp x y@(List _) = cmp (List [x]) y
cmp (Num x) (Num y) = compare x y

cmpLists :: [Element] -> [Element] -> Ordering
cmpLists [] [] = EQ
cmpLists [] _ = LT
cmpLists _ [] = GT
cmpLists (x:xs) (y:ys) = case cmp x y of
                              EQ -> cmpLists xs ys
                              o -> o

part1 :: Solution
part1 = V . sumOn' fst . filter ((== LT) . uncurry cmp . pair . map parse . snd) . zip [1..] . split null

part2 :: Solution
part2 input = let dividers = [List [List [Num 2]], List [List [Num 6]]]
                  sorted = sortBy cmp $ dividers ++ map parse (filter (not . null) input)
              in V $ productOn' fst $ filter ((`elem` dividers) . snd) $ zip [1..] sorted
