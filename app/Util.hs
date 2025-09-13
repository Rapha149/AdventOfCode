module Util where

import Data.Char

data Result = V Int | Msg String | Error String
type Solution = [String] -> Result

type Vec = (Int, Int)
type Vec3 = (Int, Int, Int)

getExtra :: (String -> Bool) -> Int -> ([String] -> a) -> a -> [String] -> (a, [String])
getExtra test n get def input | test $ hd input = (get $ take n input, drop n input)
                              | otherwise = (def, input)

getExtraInts :: Int -> ([String] -> a) -> a -> [String] -> (a, [String])
getExtraInts = getExtra (all isDigit)

getExtra1 :: (String -> Bool) -> (String -> a) -> a -> [String] -> (a, [String])
getExtra1 test get = getExtra test 1 (get . hd)

getExtraInt :: Int -> [String] -> (Int, [String])
getExtraInt = getExtra1 (all isDigit) read

hd :: [a] -> a
hd (x:_) = x
hd [] = error "hd: empty list"

hdOr :: a -> [a] -> a
hdOr a [] = a
hdOr _ xs = hd xs

tl :: [a] -> [a]
tl (_:xs) = xs
tl [] = error "tl: empty list"

tlOr :: [a] -> [a] -> [a]
tlOr a [] = a
tlOr _ xs = tl xs

lst :: [a] -> a
lst [x] = x
lst (_:xs) = lst xs
lst [] = error "lst: empty list"

lstOr :: a -> [a] -> a
lstOr a [] = a
lstOr _ xs = lst xs

ini :: [a] -> [a]
ini [_] = []
ini (x:xs) = x : ini xs
ini [] = error "ini: empty list"

iniOr :: [a] -> [a] -> [a]
iniOr a [] = a
iniOr _ xs = ini xs

pair :: [a] -> (a, a)
pair [a, b] = (a, b)
pair _ = error "Not two elements."

triple :: [a] -> (a, a, a)
triple [a, b, c] = (a, b, c)
triple _ = error "Not three elements."

onBoth :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
onBoth f (a, b) (c, d) = (f a c, f b d)

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)

inBounds :: Ord a => (a, a) -> (a, a) -> (a, a) -> Bool
inBounds (minA, minB) (maxA, maxB) (a, b) = a >= minA && a < maxA && b >= minB && b < maxB

inBounds0 :: (Num a, Ord a) => a -> a -> (a, a) -> Bool
inBounds0 maxA maxB = inBounds (0, 0) (maxA, maxB)

inBoundsBoth :: Ord a => a -> a -> (a, a) -> Bool
inBoundsBoth minA maxA = inBounds (minA, minA) (maxA, maxA)

inBoundsI :: Ord a => (a, a) -> (a, a) -> (a, a) -> Bool
inBoundsI (minA, minB) (maxA, maxB) (a, b) = a >= minA && a <= maxA && b >= minB && b <= maxB

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
