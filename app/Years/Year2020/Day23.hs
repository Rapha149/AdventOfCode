module Years.Year2020.Day23 (part1, part2) where

import Util.Util
import Data.Char
import Data.List
import Data.STRef
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV

move :: Int -> Int -> STRef s Int -> MVector s Int -> ST s ()
move mn mx current cups = do
    cVal <- readSTRef current
    x <- MV.read cups cVal
    y <- MV.read cups x
    z <- MV.read cups y
    w <- MV.read cups z

    let getDest :: Int -> Int
        getDest n | n < mn = getDest mx
                  | n `elem` [x, y, z] = getDest $ n - 1
                  | otherwise = n
        dest = getDest $ cVal - 1

    afterDest <- MV.read cups dest
    writeSTRef current w
    MV.write cups cVal w
    MV.write cups dest x
    MV.write cups z afterDest

getResult :: Int -> [Int] -> [Int]
getResult moves xs = tl $ take (length xs) $ iterate (cups V.!) 1
    where initCurrent = hd xs
          initCups = V.fromList $ 0 : map snd (sortOn fst $ zip xs (tl xs ++ [hd xs]))
          (mn, mx) = (minimum xs, maximum xs)
          cups = runST $ do
              mCups <- V.thaw initCups
              current <- newSTRef initCurrent
              replicateM_ moves (move mn mx current mCups)
              V.freeze mCups

part1 :: Solution
part1 = V . foldl (\acc v -> acc * 10 + v) 0 . getResult 100 . map digitToInt . hd

part2 :: Solution
part2 input = V $ product $ take 2 $ getResult 10000000 $ ints ++ [maximum ints + 1 .. 1000000]
    where ints = map digitToInt $ hd input
