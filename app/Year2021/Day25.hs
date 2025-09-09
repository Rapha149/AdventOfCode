module Year2021.Day25 (part1) where

import Util
import Data.List
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set

moveUntilEnd :: Int -> Int -> Set Vec -> Set Vec -> Int
moveUntilEnd height width east south | east == east' && south == south' = 1
                                     | otherwise = 1 + moveUntilEnd height width east' south'
    where east' = Set.map (move (Set.union east south) True) east
          south' = Set.map (move (Set.union east' south) False) south
          move :: Set Vec -> Bool -> Vec -> Vec
          move total dir (r, c) = let movePos | dir = (r, (c + 1) `mod` width)
                                              | otherwise = ((r + 1) `mod` height, c)
                                  in if Set.member movePos total then (r, c) else movePos

part1 :: Solution
part1 input = let total = [((r, c), x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row, x /= '.']
                  (east, south) = both (Set.fromList . map fst) $ partition ((== '>') . snd) total
                  (height, width) = (length input, length $ hd input)
              in V $ moveUntilEnd height width east south
