module Years.Year2018.Day13 (part1, part2) where

import Util.Util
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Tile = Straight | CurveCW | CurveCCW | Intersection deriving Show
type Grid = Map Vec Tile
type Carts = Map Vec (Vec, Int)

parseInput :: [String] -> (Grid, Carts)
parseInput input = (Map.map parseTile grid, Map.mapMaybe parseDir grid)
    where grid = Map.fromList [((y, x), c) | (y, row) <- zip [0..] input, (x, c) <- zip [0..] row, c /= ' ']
          parseTile :: Char -> Tile
          parseTile '/' = CurveCW
          parseTile '\\' = CurveCCW
          parseTile '+' = Intersection
          parseTile _ = Straight
          parseDir :: Char -> Maybe (Vec, Int)
          parseDir '^' = Just ((-1, 0), 0)
          parseDir 'v' = Just ((1, 0), 0)
          parseDir '<' = Just ((0, -1), 0)
          parseDir '>' = Just ((0, 1), 0)
          parseDir _ = Nothing

tick :: Grid -> Carts -> [(Vec, (Vec, Int))] -> ([Vec], Carts)
tick _ carts [] = ([], carts)
tick grid carts (((y, x), ((dy, dx), decisionIdx)):xs) | Map.notMember (y, x) carts = tick grid carts xs
                                                       | Map.member pos carts = first (pos :) $ tick grid (Map.delete pos carts') xs
                                                       | otherwise = tick grid (Map.insert pos data' carts') xs
    where carts' = Map.delete (y, x) carts
          pos = (y + dy, x + dx)
          data' = case grid Map.! pos of
                       Straight -> ((dy, dx), decisionIdx)
                       CurveCW -> ((-dx, -dy), decisionIdx)
                       CurveCCW -> ((dx, dy), decisionIdx)
                       Intersection -> (case decisionIdx of
                                             0 -> (-dx, dy)
                                             1 -> (dy, dx)
                                             2 -> (dx, -dy)
                                             _ -> error "Invalid decision index.", (decisionIdx + 1) `mod` 3)

firstCollision :: Grid -> Carts -> Vec
firstCollision grid carts = case tick grid carts $ Map.toList carts of
                                 (collision:_, _) -> collision
                                 ([], carts') -> firstCollision grid carts'

showVec :: Vec -> String
showVec (y, x) = show x ++ "," ++ show y

part1 :: Solution
part1 = Msg . showVec . uncurry firstCollision . parseInput


lastRemaining :: Grid -> Carts -> Vec
lastRemaining grid carts | length carts == 1 = fst $ Map.findMin carts
                         | otherwise = lastRemaining grid $ snd $ tick grid carts $ Map.toList carts

part2 :: Solution
part2 = Msg . showVec . uncurry lastRemaining . parseInput
