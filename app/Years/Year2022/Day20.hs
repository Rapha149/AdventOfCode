module Years.Year2022.Day20 (part1, part2) where

import Util.Util
import Data.Maybe
import Data.List.Extra
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

move :: Seq (Int, Int) -> (Int, Int) -> Seq (Int, Int)
move xs e@(_,n) = Seq.insertAt newIdx e $ Seq.deleteAt idx xs
    where idx = fromJust $ Seq.elemIndexL e xs
          newIdx = (idx + n) `mod` (Seq.length xs - 1)

getCoordinates :: Seq Int -> Int
getCoordinates xs = sumOn' (Seq.index xs . (`mod` Seq.length xs) . (zeroIdx+)) [1000, 2000, 3000]
    where zeroIdx = fromJust $ Seq.elemIndexL 0 xs

part1 :: Solution
part1 input = let elements = zip [0..] $ map read input
              in V $ getCoordinates $ snd <$> foldl move (Seq.fromList elements) elements

part2 :: Solution
part2 input = let elements = zip [0..] $ map ((* 811589153) . read) input
              in V $ getCoordinates $ snd <$> foldr ($) (Seq.fromList elements) (replicate 10 (\xs -> foldl move xs elements))
