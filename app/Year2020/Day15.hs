module Year2020.Day15 (part1, part2) where

import Util
import Data.List.Split
import Control.Monad
import Control.Monad.ST
import Control.Monad.Loops
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as V

getResult :: Int -> [String] -> Int
getResult goal input = runST $ do
    let nums = map read $ splitOn "," $ hd input
    turn <- newSTRef $ length nums
    num <- newSTRef $ lst nums
    history <- V.replicate goal 0
    forM_ (zip (ini nums) [1..]) $ uncurry (V.write history)

    whileM_ ((/= goal) <$> readSTRef turn) $ speak turn num history
    readSTRef num

speak :: STRef s Int -> STRef s Int -> V.MVector s Int -> ST s ()
speak turn num history = do
    turnVal <- readSTRef turn
    numVal <- readSTRef num
    numHist <- V.read history numVal
    let next = case numHist of
                    0 -> 0
                    t -> turnVal - t
    V.write history numVal turnVal
    modifySTRef turn (+ 1)
    writeSTRef num next
    return ()

part1 :: Solution
part1 = V . getResult 2020

part2 :: Solution
part2 = V . getResult 30000000
