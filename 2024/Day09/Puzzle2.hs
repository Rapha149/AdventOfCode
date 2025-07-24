module Main where
import Data.Char
import qualified Data.Map as Map

parseContent :: String -> Int -> Int -> Map.Map Int (Int, Int) -> Map.Map Int Int -> (Map.Map Int (Int, Int), Map.Map Int Int)
parseContent [] _ _ ids free = (ids, free)
parseContent (b:f:cs) i bI ids free = parseContent cs (i + blockSize + freeSize) (bI + 1) (Map.insert i (bI, blockSize) ids) (Map.insert (i + blockSize) freeSize free)
    where blockSize = digitToInt b
          freeSize = digitToInt f
parseContent (b:cs) i bI ids free = parseContent cs (i + blockSize) (bI + 1) (Map.insert i (bI, blockSize) ids) free
    where blockSize = digitToInt b

moveBlock :: Map.Map Int (Int, Int) -> Int -> Int -> Int -> Map.Map Int Int -> [Int] -> (Map.Map Int (Int, Int), Map.Map Int Int)
moveBlock ids _ _ _ free [] = (ids, free)
moveBlock ids blockIndex blockId blockSize free (freeIndex:freeIndices) | blockIndex <= freeIndex = (ids, free)
                                                                        | blockSize > freeSize = moveBlock ids blockIndex blockId blockSize free freeIndices
                                                                        | blockSize == freeSize = (Map.insert freeIndex (blockId, blockSize) $ Map.delete blockIndex ids, Map.delete freeIndex free)
                                                                        | otherwise = (Map.insert freeIndex (blockId, blockSize) $ Map.delete blockIndex ids, Map.insert (freeIndex + blockSize) (freeSize - blockSize) $ Map.delete freeIndex free)
    where freeSize = free Map.! freeIndex

moveBlocks :: Map.Map Int (Int, Int) -> [Int] -> Map.Map Int Int -> Map.Map Int (Int, Int)
moveBlocks ids indices free | blockIndex < 1 = ids
                            | otherwise = let (newIds, newFree) = moveBlock ids blockIndex blockId blockSize free $ Map.keys free
                                          in moveBlocks newIds (tail indices) newFree
    where blockIndex = head indices
          (blockId, blockSize) = ids Map.! blockIndex

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (blocks, freeSpace) = parseContent (head $ lines content) 0 0 Map.empty Map.empty
        indices = reverse $ Map.keys blocks
        moved = moveBlocks blocks indices freeSpace
    print $ Map.foldrWithKey (\k v acc -> (sum $ map (* fst v) [k .. k + snd v - 1]) + acc) 0 moved
