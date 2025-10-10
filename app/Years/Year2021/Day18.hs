module Years.Year2021.Day18 (part1, part2) where

import Util.Util
import Data.Char
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Number = Regular { depth :: Int, value :: Int } | Pair { depth :: Int } deriving Show

parseNumber :: Int -> String -> Seq Number
parseNumber 0 "" = Seq.empty
parseNumber _ "" = error "No chars left but not depth 0."
parseNumber depth ('[':cs) = Pair {..} Seq.<| parseNumber (depth + 1) cs
parseNumber depth (']':cs) = parseNumber (depth - 1) cs
parseNumber depth (',':cs) = parseNumber depth cs
parseNumber _ (c:_) | not $ isDigit c = error "Not a valid number."
parseNumber depth cs = Regular {..} Seq.<| parseNumber depth rest
    where (value, rest) = first read $ span isDigit cs

reduce :: Seq Number -> Seq Number
reduce s | isJust toExplode = let idx = fromJust toExplode
                                  (left, right) = both (\case Regular {..} -> value; Pair {} -> error "Left/right element is a pair.") (s `Seq.index` (idx + 1), s `Seq.index` (idx + 2))
                                  adjust :: ((Number -> Bool) -> Seq Number -> Maybe Int) -> Int -> Seq Number -> Seq Number
                                  adjust f v s' = case f (\case Regular {} -> True; Pair {} -> False) s' of
                                                       Just i -> Seq.adjust' (\case reg@Regular {..} -> reg { value = value + v}; Pair {} -> error "Element is a pair.") i s'
                                                       Nothing -> s'
                                  (before, after) = Seq.splitAt idx s
                                  (before', after') = (adjust Seq.findIndexR left before, adjust Seq.findIndexL right $ Seq.drop 3 after)
                              in reduce $ before' Seq.>< Seq.singleton Regular { depth = 4, value = 0 } Seq.>< after'
         | isJust toSplit = let idx = fromJust toSplit
                                (before, after) = Seq.splitAt idx s
                                new = case after `Seq.index` 0 of
                                           Regular {..} -> Seq.fromList [Pair {..},
                                                                         Regular { depth = depth + 1, value = value `div` 2 },
                                                                         Regular { depth = depth + 1, value = (value + 1) `div` 2 }]
                                           Pair {} -> error "Element is a pair."
                            in reduce $ before Seq.>< new Seq.>< Seq.drop 1 after
         | otherwise = s
    where toExplode = Seq.findIndexL (\case Regular {} -> False; Pair {..} -> depth == 4) s
          toSplit = Seq.findIndexL (\case Regular {..} -> value >= 10; Pair {} -> False) s

add :: Seq Number -> Seq Number -> Seq Number
add s1 s2 = reduce $ Pair { depth = 0 } Seq.<| fmap (\n -> n { depth = n.depth + 1}) (s1 Seq.>< s2)

getMagnitude :: Seq Number -> Int
getMagnitude s = case s `Seq.index` 0 of
                      Regular {..} -> value
                      Pair {..} -> sumOn' (uncurry (*) . second (getMagnitude . (`Seq.drop` s))) $ zip [3,2] $ Seq.findIndicesL (\n -> n.depth == depth + 1) s

part1 :: Solution
part1 = V . getMagnitude . foldl1 add . map (parseNumber 0)

part2 :: Solution
part2 input = V $ maximum [getMagnitude $ add s1 s2 | (i1, s1) <- numbers, (i2, s2) <- numbers, i1 /= i2]
    where numbers = zip [0 :: Int ..] $ map (parseNumber 0) input
