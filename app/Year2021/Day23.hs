module Year2021.Day23 (part1, part2) where

import Util
import Data.Bits
import Data.Word
import Data.Maybe
import Data.List
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ

type State = (Word32, Word32, Word32)

clearType :: Int -> State -> State
clearType i (occ, t0, t1) = (clearBit occ i, clearBit t0 i, clearBit t1 i)

setType :: Int -> Int -> State -> State
setType i p (occ, t0, t1) = (setBit occ i, if even p then t0 else setBit t0 i, if p < 2 then t1 else setBit t1 i)

getType :: Int -> State -> Int
getType i (_, t0, t1) = fromEnum (testBit t0 i) + fromEnum (testBit t1 i) * 2

parseInput :: [String] -> State
parseInput input = foldr (uncurry setType) (0, 0, 0) pods
    where pods = concat $ zipWith (\b -> zip [b..] . map (fromJust . (`elemIndex` "ABCD"))) [11, 15, 19, 23] $ filter (notElem '#') $ transpose $ ini $ drop 2 input

hallSpots :: [Int]
hallSpots = [0, 1, 3, 5, 7, 9, 10]

roomIdx :: Int -> Int
roomIdx r = (r + 1) * 2

organized :: Int -> State
organized depth = foldr (uncurry setType) (0, 0, 0) [(11 + r * 4 + d, r) | r <- [0..3], d <- [0.. depth]]

roomMask :: Int -> Word32
roomMask room = shiftL 0b1111 $ room * 4 + 11

typeMask :: Int -> State -> Word32
typeMask t (occ, t0, t1) = case t of
                                0 -> occ .&. complement t0 .&. complement t1
                                1 -> occ .&. t0 .&. complement t1
                                2 -> occ .&. complement t0 .&. t1
                                3 -> occ .&. t0 .&. t1
                                _ -> error "Invalid type."

betweenHall :: Int -> Int -> Word32
betweenHall a b = foldl setBit 0 [min a b .. max a b]

getMinEnergy :: Int -> Map State Int -> PQueue Int State -> Int
getMinEnergy maxDepth seen queue | PQ.null queue = error "No solution found."
                                 | state == organized maxDepth = cost
                                 | maybe False (<= cost) $ seen Map.!? state = getMinEnergy maxDepth seen rest
                                 | otherwise = getMinEnergy maxDepth (Map.insert state cost seen) (foldr (uncurry PQ.insert) rest next)
    where ((cost, state@(occ, _, _)), rest) = fromJust $ PQ.minViewWithKey queue
          next = map (first (+ cost)) $ concatMap getNext $ filter (testBit occ) [0..27]
          isClean :: Int -> Bool
          isClean r = let rMask = roomMask r
                      in occ .&. rMask == typeMask r state .&. rMask
          getNext :: Int -> [(Int, State)]
          getNext i = let t = getType i state
                          state' = clearType i state
                      in (if i <= 10 then getNextHall else getNextRoom) i t state'
          getNextHall :: Int -> Int -> State -> [(Int, State)]
          getNextHall hIdx t state'@(occ', _, _) = [(10 ^ t * (abs (hIdx - rIdx) + depth + 1), setType (t * 4 + 11 + depth) t state') |
                                                       let rIdx = roomIdx t, occ' .&. betweenHall hIdx rIdx == 0, -- way is clear
                                                       isClean t,
                                                       let depth = fromJust $ find (not . testBit occ . (t * 4 + 11 +)) $ reverse [0..maxDepth]]
          getNextRoom :: Int -> Int -> State -> [(Int, State)]
          getNextRoom i t state'@(_, _, _) = [(10 ^ t * (abs (hIdx - rIdx) + depth + 1), setType hIdx t state') |
                                                 let room = (i - 11) `div` 4, let depth = (i - 11) `mod` 4,
                                                 occ .&. foldl setBit 0 [room * 4 + 11 .. i - 1] == 0, -- above in room is free
                                                 not $ isClean room,
                                                 let rIdx = roomIdx room, hIdx <- hallSpots,
                                                 occ .&. betweenHall hIdx rIdx == 0]

part1 :: Solution
part1 = V . getMinEnergy 1 Map.empty . PQ.singleton 0 . parseInput

part2 :: Solution
part2 input = let newInput = take 3 input ++ ["  #D#C#B#A#", "  #D#B#A#C#"] ++ drop 3 input
              in V $ getMinEnergy 3 Map.empty $ PQ.singleton 0 $ parseInput newInput
