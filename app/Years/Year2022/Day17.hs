module Years.Year2022.Day17 (part1, part2) where

import Util.Util
import Data.Bits
import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Shaft = IntMap Int
type Shape = ([Int], Int)
data State = S { shaft :: Shaft, height :: Int, shapeIdx :: Int, jetIdx :: Int }

width, rowCount, newLine :: Int
width = 9
rowCount = 8
newLine = 0b100000001

initState :: State
initState = S { shaft = IM.singleton 0 0b111111111, height = 0, shapeIdx = 0, jetIdx = 0 }

shapes :: [Shape]
shapes = map (\b -> (b, maximum $ map (\x -> finiteBitSize x - countLeadingZeros x) b)) bitMaps
    where bitMaps = [[0b1111], [0b010, 0b111, 0b010], [0b111, 0b001, 0b001], [1, 1, 1, 1], [0b11, 0b11]]

xshift :: Int -> Int -> Int -> Int
xshift v w x = shiftL v $ width - x - (w - 1)

draw :: Shaft -> Shape -> Vec -> Shaft
draw shaft (bits, w) (x, y) = foldr (\(i, b) -> IM.alter (\l -> Just $ fromMaybe newLine l `xor` xshift b w x) (y + i)) shaft $ zip [0..] bits

collides :: Shaft -> Shape -> Vec -> Bool
collides shaft (bits, w) (x, y) = or $ zipWith (\i b -> IM.findWithDefault newLine (y + i) shaft .&. xshift b w x /= 0) [0..] bits

move :: Shaft -> Shape -> Vec -> Vec -> (Shaft, Vec, Bool)
move shaft shape old new | collides shaft' shape new = (shaft, old, False)
                         | otherwise = (draw shaft' shape new, new, True)
    where shaft' = draw shaft shape old

addRock :: String -> State -> State
addRock jets (S {..}) = let pos = (4, height + 4)
                            (shaft', (_, y), j) = fall (draw shaft shape pos) pos jetIdx
                        in S { shaft = shaft', height = max height $ y + length (fst shape) - 1,
                               shapeIdx = (shapeIdx + 1) `mod` length shapes, jetIdx = j }
    where shape = shapes !! shapeIdx
          fall :: Shaft -> Vec -> Int -> (Shaft, Vec, Int)
          fall s p@(x, y) j = let (s', p'@(x', _), _) = move s shape p (x + if jets !! j == '<' then (-1) else 1, y)
                                  (s'', p'', moved) = move s' shape p' (x', y - 1)
                                  j' = (j + 1) `mod` length jets
                              in if moved then fall s'' p'' j' else (s'', p'', j')

getHeightAfter :: Int -> String -> Int
getHeightAfter n jets = height $ foldr ($) initState (replicate n $ addRock jets)

findLoop :: String -> State -> Int -> Map (Int, Int, Int) (Int, Int) -> (Int, Int, Int)
findLoop jets state@S {..} n states | coversWidth = case states Map.!? key of
                                                         Just (n', h) -> (n', n - n', height - h)
                                                         Nothing -> findLoop jets state' (n + 1) $ Map.insert key (n, height) states
                                    | otherwise = findLoop jets state' (n + 1) states
    where coversWidth = height >= rowCount && foldr ((.|.) . (shaft IM.!) . (height -)) 0 [0..rowCount-1] == 0b111111111
          encoded = foldr (\i acc -> (shaft IM.! height - i) .&. 0b011111110 + acc * 8) 0 [0..rowCount-1]
          key = (encoded, shapeIdx, jetIdx)
          state' = addRock jets state

part1 :: Solution
part1 = V . getHeightAfter 2022 . hd

part2 :: Solution
part2 input = let jets = hd input
                  (pre, loopLen, loopHeight) = findLoop jets initState 0 Map.empty
                  (q, r) = (1000000000000 - pre) `divMod` loopLen
              in V $ getHeightAfter (pre + r) jets + q * loopHeight
