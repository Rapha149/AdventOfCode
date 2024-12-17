module Main where
import Data.Char
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict

insertBlocks :: Int -> Int -> Int -> Map.Map Int Int -> Map.Map Int Int
insertBlocks _ _ 0 ids = ids
insertBlocks i bI n ids = insertBlocks (i + 1) bI (n - 1) $ Map.insert i bI ids

insertFreeSpace :: Int -> Int -> [Int] -> [Int]
insertFreeSpace _ 0 free = free
insertFreeSpace i n free = insertFreeSpace (i + 1) (n - 1) $ i : free

parseContent :: String -> Int -> Int -> Map.Map Int Int -> [Int] -> (Map.Map Int Int, [Int])
parseContent [] _ _ ids free = (ids, reverse free)
parseContent (b:f:cs) i bI ids free = parseContent cs (i + blockSize + freeSize) (bI + 1) (insertBlocks i bI blockSize ids) (insertFreeSpace (i + blockSize) freeSize free)
    where blockSize = digitToInt b
          freeSize = digitToInt f
parseContent (b:cs) i bI ids free = parseContent cs (i + blockSize) (bI + 1) (insertBlocks i bI blockSize ids) free
    where blockSize = digitToInt b

moveBlocks :: Map.Map Int Int -> [Int] -> [Int] -> Map.Map Int Int
moveBlocks ids indices free | blockIndex > freeIndex = moveBlocks (Map.insert freeIndex blockId $ Map.delete blockIndex ids) (tail indices) (tail free)
                            | otherwise = ids
    where blockIndex = head indices
          freeIndex = head free
          blockId = ids Map.Strict.! blockIndex

main :: IO ()
main = do
    content <- readFile "input.txt"
    let (blocks, freeSpace) = parseContent (head $ lines content) 0 0 Map.empty []
        indices = reverse $ Map.keys blocks
        moved = moveBlocks blocks indices freeSpace
    print $ Map.Strict.foldrWithKey (\k v acc -> k * v + acc) 0 moved
