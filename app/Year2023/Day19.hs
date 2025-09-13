module Year2023.Day19 (part1, part2) where

import Util
import Data.List.Split
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Target = Name String | A | R
data Rule = Cond Char (Int -> Int -> Bool) Int Target | Direct Target
type Workflows = Map String [Rule]
type PartRatings = Map Char Int
type PartRanges = Map Char (Int, Int)

parseWorkflow :: String -> (String, [Rule])
parseWorkflow workflow = (name, map parseRule $ splitOn "," rules)
    where (name, rules) = pair $ splitOn "{" $ ini workflow
          parseRule :: String -> Rule
          parseRule rule = case splitOn ":" rule of
                                [t] -> Direct $ parseTarget t
                                [c:o:n, t] -> Cond c (if o == '>' then (>) else (<)) (read n) $ parseTarget t
                                _ -> error "Invalid rule."
          parseTarget :: String -> Target
          parseTarget "A" = A
          parseTarget "R" = R
          parseTarget a = Name a

parsePart :: String -> PartRatings
parsePart = Map.fromList . map parseRating . splitOn "," . tl . ini
    where parseRating :: String -> (Char, Int)
          parseRating (c:'=':n) = (c, read n)
          parseRating _ = error "Invalid rating."

isAccepted :: Workflows -> (PartRatings, Target) -> Bool
isAccepted _ (_, A) = True
isAccepted _ (_, R) = False
isAccepted workflows (part, Name name) = isAccepted workflows (part, getTarget $ workflows Map.! name)
    where getTarget :: [Rule] -> Target
          getTarget [] = error "No rules left."
          getTarget (Direct target : _) = target
          getTarget (Cond category cmp number target : rs) | part Map.! category `cmp` number = target
                                                           | otherwise = getTarget rs

getAcceptedCombinations :: Workflows -> (PartRanges, Target) -> Int
getAcceptedCombinations _ (ranges, A) = Map.foldr (*) 1 $ Map.map (\(a, b) -> b - a + 1) ranges
getAcceptedCombinations _ (_, R) = 0
getAcceptedCombinations workflows (r, Name name) = sum $ map (getAcceptedCombinations workflows) $ splitRanges r (workflows Map.! name)
    where splitRanges :: PartRanges -> [Rule] -> [(PartRanges, Target)]
          splitRanges _ [] = error "No rules left."
          splitRanges ranges (Direct target : _) = [(ranges, target)]
          splitRanges ranges (Cond category cmp number target : rs) = let (a, b) = ranges Map.! category
                                                                          newRange rnge = Map.insert category rnge ranges
                                                                      in case (a `cmp` number, b `cmp` number) of
                                                                              (False, False) -> splitRanges ranges rs
                                                                              (True, True) -> [(ranges, target)]
                                                                              (False, True) -> (newRange (number + 1, b), target) : splitRanges (newRange (a, number)) rs
                                                                              (True, False) -> (newRange (a, number - 1), target) : splitRanges (newRange (number, b)) rs

part1 :: Solution
part1 input = let (workflows, parts) = bimap (Map.fromList . map parseWorkflow) (map parsePart) $ pair $ splitOn [""] input
              in V $ sum $ map (Map.foldr (+) 0 . fst) $ filter (isAccepted workflows) $ map (, Name "in") parts

part2 :: Solution
part2 input = let workflows = Map.fromList $ map parseWorkflow $ hd $ splitOn [""] input
              in V $ getAcceptedCombinations workflows (Map.fromList $ map (, (1, 4000)) "xmas", Name "in")
