module Years.Year2024.Day09 (part1, part2) where

import Util.Util
import Data.Char
import Data.Maybe
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Block = ID Int | Free deriving (Eq, Show)
type BlockLengths = Map Int (Block, Int)
type Blocks = Map Int Block

blockToId :: Block -> Int
blockToId (ID a) = a
blockToId Free = error "Not a file block."

parseBlockLengths :: String -> BlockLengths
parseBlockLengths line = snd $ foldl insertBlock (0, Map.empty) blocks
    where blocks = zipWith (\l n -> (if even n then ID (n `div` 2) else Free, digitToInt l)) line [0..]
          insertBlock :: (Int, BlockLengths) -> (Block, Int) -> (Int, BlockLengths)
          insertBlock (pos, m) (b, len) = (pos + len, Map.insert pos (b, len) m)

toIndividualBlocks :: [(Int, (Block, Int))] -> Blocks
toIndividualBlocks [] = Map.empty
toIndividualBlocks ((pos, (b, len)):xs) = foldr (\i -> Map.insert (pos + i) b) (toIndividualBlocks xs) [0..len - 1]

fillFreeIndividual :: Blocks -> [Int] -> Blocks
fillFreeIndividual blocks [] = blocks
fillFreeIndividual blocks (free:fs) | pos > free = Map.insert free b $ fillFreeIndividual rest fs
                                    | otherwise = blocks
    where ((pos, b), rest) = fromJust $ Map.maxViewWithKey blocks

part1 :: Solution
part1 input = V $ sum $ map (\(pos, b) -> pos * blockToId b) $ Map.toList filled
    where blocks = toIndividualBlocks $ Map.toList $ parseBlockLengths $ hd input
          filled = fillFreeIndividual (Map.filter (/= Free) blocks) (Map.keys $ Map.filter (== Free) blocks)


fillFreeFiles :: BlockLengths -> [(Int, Int)] -> [(Int, (Block, Int))] -> BlockLengths
fillFreeFiles blocks [] _ = blocks
fillFreeFiles blocks _ [] = blocks
fillFreeFiles blocks free ((pos, (b, len)):xs) | isNothing result = fillFreeFiles blocks free xs
                                               | otherwise = let (p, rest) = fromJust result
                                                             in fillFreeFiles (Map.insert p (b, len) $ Map.delete pos blocks) rest xs
    where findFree :: [(Int, Int)] -> Maybe (Int, [(Int, Int)])
          findFree [] = Nothing
          findFree ((p, l):fs) | pos < p = Nothing
                               | l == len = Just (p, fs)
                               | l > len = Just (p, (p + len, l - len):fs)
                               | otherwise = second ((p,l):) <$> findFree fs
          result = findFree free

part2 :: Solution
part2 input = V $ sum $ map (\(pos, (b, len)) -> sum (take len [pos..]) * blockToId b) $ Map.toList filled
    where blocks = parseBlockLengths $ hd input
          files = Map.filter ((/= Free) . fst) blocks
          filled = fillFreeFiles files (Map.toAscList $ Map.map snd $ Map.filter ((== Free) . fst) blocks) (Map.toDescList files)
