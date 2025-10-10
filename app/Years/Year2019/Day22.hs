module Years.Year2019.Day22 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Maybe

data Shuffle = DealStack | Cut Int | DealIncrement Int deriving (Show, Eq)

parseInput :: [String] -> [Shuffle]
parseInput = map (parseShuffle . words)
    where parseShuffle :: [String] -> Shuffle
          parseShuffle ["deal", "into", "new", "stack"] = DealStack
          parseShuffle ["cut", n] = Cut $ read n
          parseShuffle ["deal", "with", "increment", n] = DealIncrement $ read n
          parseShuffle _ = error "Unknown shuffle technique."

shuffle :: [Int] -> Shuffle -> [Int]
shuffle xs DealStack = reverse xs
shuffle xs (Cut n) = after ++ before
    where (before, after) | n >= 0 = splitAt n xs
                          | otherwise = splitAtEnd (abs n) xs
shuffle xs (DealIncrement n) = map fst $ sortOn snd $ zip xs $ iterate ((`mod` length xs) . (+ n)) 0

part1 :: Solution
part1 raw | raw == input = V $ fromJust $ 2019 `elemIndex` cards
          | otherwise = Msg $ unwords $ map show cards
    where (amount, input) = getExtraInt 10007 raw
          shuffles = parseInput input
          cards = foldl shuffle [0..amount - 1] shuffles


-- Part 2 solution explanation: https://www.reddit.com/r/adventofcode/comments/ee56wh/comment/fbr0vjb/

mulMod :: Int -> Int -> Int -> Int
mulMod a b m = fromInteger $ fromIntegral a * fromIntegral b `mod` fromIntegral m

-- Compact "deal into stack" shuffles.
-- Two of these cancel each other out -> traverse the list and flip reversed every time we encounter one.
-- At the end, if reversed is true, add a "deal into stack" shuffle.
-- For the other shuffles, if it's currently reversed, modify them like this:
--      deal into new stack
--      cut x
--      ->
--      cut -x
--      deal into new stack
--
--      deal into new stack
--      deal with increment x
--      ->
--      deal with increment x
--      cut -x+1
--      deal into new stack
compactDealStack :: Bool -> [Shuffle] -> [Shuffle]
compactDealStack False [] = []
compactDealStack True [] = [DealStack]
compactDealStack reversed (DealStack:xs) = compactDealStack (not reversed) xs
compactDealStack False (x:xs) = x : compactDealStack False xs
compactDealStack True (Cut n:xs) = Cut (-n) : compactDealStack True xs
compactDealStack True (DealIncrement n:xs) = DealIncrement n : Cut (-n+1) : compactDealStack True xs

-- Compact "cut" shuffles.
-- "Deal into stack" shuffles need to be compacted already, so that the final "cut" shuffle can be inserted right before the last "deal into stack" shuffle.
-- Modify the shuffles like this:
--      cut x
--      cut y
--      ->
--      cut (x+y) % count
--
--      cut x
--      deal with increment y
--      ->
--      deal with increment y
--      cut (x*y) % count
compactCut :: Int -> Int -> [Shuffle] -> [Shuffle]
compactCut _ 0 [] = []
compactCut _ cut [] = [Cut cut]
compactCut amount cut (Cut n:xs) = compactCut amount ((cut + n) `mod` amount) xs
compactCut amount 0 (x:xs) = x : compactCut amount 0 xs
compactCut amount cut (DealStack:xs) = Cut cut : DealStack : compactCut amount 0 xs
compactCut amount cut (DealIncrement n:xs) = DealIncrement n : compactCut amount (mulMod cut n amount) xs

-- Compact "deal with increment" shuffles.
-- Finally, we only have to combine the "deal with increment" shuffles like this:
--      deal with increment x
--      deal with increment y
--      ->
--      deal with increment (x*y) % count
compactDealIncrement :: Int -> Int -> [Shuffle] -> [Shuffle]
compactDealIncrement _ 1 [] = []
compactDealIncrement _ increment [] = [DealIncrement increment]
compactDealIncrement amount increment (DealIncrement n:xs) = compactDealIncrement amount (mulMod increment n amount) xs
compactDealIncrement amount 1 (x:xs) = x : compactDealIncrement amount 1 xs
compactDealIncrement amount increment (x:xs) = DealIncrement increment : x : compactDealIncrement amount 1 xs

compact :: Int -> [Shuffle] -> [Shuffle]
compact amount = compactDealIncrement amount 1 . compactCut amount 0 . compactDealStack False

exponentiate :: Int -> [Shuffle] -> [Shuffle] -> Int -> [Shuffle]
exponentiate _ _ shuffles 0 = shuffles
exponentiate amount factor shuffles n = exponentiate amount factor' shuffles' $ n `div` 2
    where shuffles' | odd n = compact amount $ shuffles ++ factor
                    | otherwise = shuffles
          factor' = compact amount $ factor ++ factor

getCardInPos :: Int -> [Shuffle] -> Int -> Int
getCardInPos _ [] pos = pos
getCardInPos amount (DealStack:xs) pos = getCardInPos amount xs $ amount - 1 - pos
getCardInPos amount (Cut n:xs) pos = getCardInPos amount xs $ (pos - n) `mod` amount
getCardInPos amount (DealIncrement n:xs) pos = getCardInPos amount xs $ mulMod pos n amount

part2 :: Solution
part2 input = V $ getCardInPos amount iterated 2020
    where amount = 119315717514047
          iterations = 101741582076661
          shuffles = parseInput input
          compacted = compact amount shuffles
          iterated = exponentiate amount compacted [] $ amount - 1 - iterations
