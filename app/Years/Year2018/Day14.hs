module Years.Year2018.Day14 (part1, part2) where

import Util.Util
import Data.Char
import Data.Maybe
import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data State = State { i1 :: Int, i2 :: Int, scores :: Seq Int }

createNew :: State -> State
createNew State{..} = State { i1 = (i1 + s1 + 1) `mod` len, i2 = (i2 + s2 + 1) `mod` len, scores = scores' }
    where s1 = scores `Seq.index` i1
          s2 = scores `Seq.index` i2
          digits = map digitToInt $ show $ s1 + s2
          scores' = scores Seq.>< Seq.fromList digits
          len = length scores'

part1 :: Solution
part1 input = V $ foldl (\acc d -> acc * 10 + d) 0 $ Seq.take 10 $ fromJust $ find ((>= 10) . length) $ map (Seq.drop after . scores) $ iterate createNew $ State 0 1 (Seq.fromList [3, 7])
    where after = read $ hd input


findSubseqIndex :: Seq Int -> Seq Int -> Maybe Int
findSubseqIndex subseq scores | Seq.drop (len - sublen - 2) scores == subseq = Just $ len - sublen - 2
                              | Seq.take sublen (Seq.drop (len - sublen - 3) scores) == subseq = Just $ len - sublen - 3
                              | otherwise = Nothing
    where len = length scores
          sublen = length subseq

part2 :: Solution
part2 input = V $ hd $ mapMaybe (findSubseqIndex subseq . scores) $ iterate createNew $ State 0 1 (Seq.fromList [3, 7])
    where subseq = Seq.fromList $ map digitToInt $ hd input
