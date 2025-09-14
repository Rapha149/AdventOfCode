module Year2024.Day15 (part1, part2) where

import Util
import Data.List.Extra
import Data.Maybe
import Control.Applicative
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Obstacles = Map Vec Bool -- point -> movable

parseMap :: [String] -> (Obstacles, Vec)
parseMap input = (Map.fromList $ map (second (== 'O')) $ filter ((`elem` "#O") . snd) chars, fst $ fromJust $ find ((== '@') . snd) chars)
    where chars = [((r, c), x) | (r, row) <- zip [0..] input, (c, x) <- zip [0..] row]

parseMoves :: String -> [Vec]
parseMoves = map (\case
                  '^' -> (-1, 0)
                  'v' -> (1, 0)
                  '<' -> (0, -1)
                  '>' -> (0, 1)
                  _ -> error "Unknown direction.")

moveObstacle :: Obstacles -> Vec -> Vec -> Maybe Obstacles
moveObstacle obstacles obstacle delta = Map.delete obstacle <$> move obstacle
    where move :: Vec -> Maybe Obstacles
          move o = let next = onBoth (+) o delta
                   in case obstacles Map.!? next of
                           Nothing -> Just $ Map.insert next True obstacles
                           (Just True) -> move next
                           (Just False) -> Nothing

moveRobot :: Obstacles -> Vec -> [Vec] -> Obstacles
moveRobot obstacles _ [] = obstacles
moveRobot obstacles robot (delta:ds) | Map.notMember next obstacles = moveRobot obstacles next ds
                                     | not $ obstacles Map.! next = moveRobot obstacles robot ds
                                     | otherwise = case moveObstacle obstacles next delta of
                                                        (Just newObstacles) -> moveRobot newObstacles next ds
                                                        Nothing -> moveRobot obstacles robot ds
    where next = onBoth (+) robot delta

getWideObstacle :: Obstacles -> Vec -> Maybe (Vec, Bool)
getWideObstacle obstacles (r, c) = get (r, c) <|> get (r, c - 1)
    where get p = (p,) <$> obstacles Map.!? p

moveWideObstacle :: Obstacles -> Vec -> Vec -> Maybe Obstacles
moveWideObstacle obstacles obstacle@(r, c) delta@(dr, dc) = Map.insert moved True . Map.delete obstacle <$> move next
    where moved = onBoth (+) obstacle delta
          next = filter (`Map.member` obstacles) $ if dr == 0 then [(r, c + 2 * dc)] else [(r + dr, c - 1), (r + dr, c), (r + dr, c + 1)]
          move :: [Vec] -> Maybe Obstacles
          move [] = Just obstacles
          move [n] | obstacles Map.! n = moveWideObstacle obstacles n delta
                   | otherwise = Nothing
          move [n1, n2] | obstacles Map.! n1 && obstacles Map.! n2 = case moveWideObstacle obstacles n1 delta of
                                                                          (Just os) -> moveWideObstacle os n2 delta
                                                                          Nothing -> Nothing
                        | otherwise = Nothing
          move _ = error "Illegal number of adjacent obstacles."

moveRobotWide :: Obstacles -> Vec -> [Vec] -> Obstacles
moveRobotWide obstacles _ [] = obstacles
moveRobotWide obstacles robot (delta:ds) | isNothing obstacle = moveRobotWide obstacles next ds
                                         | not $ snd $ fromJust obstacle = moveRobotWide obstacles robot ds
                                         | otherwise = case moveWideObstacle obstacles (fst $ fromJust obstacle) delta of
                                                            (Just newObstacles) -> moveRobotWide newObstacles next ds
                                                            Nothing -> moveRobotWide obstacles robot ds
    where next = onBoth (+) robot delta
          obstacle = getWideObstacle obstacles next

sumCoordinates :: Obstacles -> Int
sumCoordinates = Map.foldrWithKey (\(r, c) b -> (+) $ if b then r * 100 + c else 0) 0

part1 :: Solution
part1 input = let ((obstacles, start), moves) = bimap parseMap (parseMoves . concat) $ pair $ split null input
              in V $ sumCoordinates $ moveRobot obstacles start moves

part2 :: Solution
part2 input = let ((obstacles, start), moves) = bimap parseMap (parseMoves . concat) $ pair $ split null input
                  wideObstacles = Map.mapKeys (second (*2)) obstacles
              in V $ sumCoordinates $ moveRobotWide wideObstacles (second (*2) start) moves
