module Main where
import Data.List
import qualified Data.Map as Map

type Buttons = Map.Map Char Int

data RobotState = RobotState { pos :: Map.Map Int Char, moves :: Int } deriving Show
defaultState = RobotState { pos = Map.fromList $ zip [0..] $ replicate 3 'A', moves = 0 }

buttonToPos :: Int -> Char -> (Int, Int)
buttonToPos 0 button = case button of
                            '7' -> (0, 0)
                            '8' -> (1, 0)
                            '9' -> (2, 0)
                            '4' -> (0, 1)
                            '5' -> (1, 1)
                            '6' -> (2, 1)
                            '1' -> (0, 2)
                            '2' -> (1, 2)
                            '3' -> (2, 2)
                            '0' -> (1, 3)
                            'A' -> (2, 3)
buttonToPos _ button = case button of
                            '^' -> (1, 0)
                            'A' -> (2, 0)
                            '<' -> (0, 1)
                            'v' -> (1, 1)
                            '>' -> (2, 1)

getButtonsToNavigate :: Int -> Char -> Char -> Buttons
getButtonsToNavigate robot current target = Map.fromList $ filter (\(c, n) -> n > 0) [(if xDiff >= 0 then '>' else '<', abs xDiff), (if yDiff >= 0 then 'v' else '^', abs yDiff)]
    where (x1, y1) = buttonToPos robot current
          (x2, y2) = buttonToPos robot target
          xDiff = x2 - x1
          yDiff = y2 - y1

isMoveAllowed :: Int -> RobotState -> Char -> Int -> Bool
isMoveAllowed 0 _ _ _ = True
isMoveAllowed robot' state button n = newPos /= if robot == 0 then (0, 3) else (0, 0)
    where robot = robot' - 1
          currentButton = pos state Map.! robot
          (x, y) = buttonToPos robot currentButton
          newPos = case button of
                          '^' -> (x, y - n)
                          'v' -> (x, y + n)
                          '<' -> (x - n, y)
                          '>' -> (x + n, y)
                          _   -> (x, y)

getButton :: Int -> RobotState -> Buttons -> Bool -> (Char, Int)
getButton robot state buttons useMin | length buttons == 1 = (target, n)
                                     | otherwise = if isMoveAllowed robot state target n then (target, n) else getButton robot state buttons' useMin
    where Just ((target, n), buttons') = if useMin then Map.minViewWithKey buttons else Map.maxViewWithKey buttons

pressButtons :: Int -> Buttons -> Bool -> RobotState -> RobotState
pressButtons 3 buttons _ state = state { moves = moves state + Map.foldr (+) 0 buttons + 1 }
pressButtons robot buttons pressA state | Map.null buttons = let state' = if pressA then pressButtons (robot + 1) (getButtonsToNavigate robot current 'A') True state else state
                                                             in if pressA then state' { pos = Map.insert robot 'A' $ pos state' } else state
                                        | Map.member current buttons && isMoveAllowed robot state current (buttons Map.! current) =
                                            pressButtons robot (decreaseButton current $ buttons Map.! current) pressA $ pressButtons (robot + 1) Map.empty True state
                                        | otherwise = let states = sortOn (\s -> moves s) $ pressButtons' True
                                                      in states !! 0
    where current = pos state Map.! robot
          decreaseButton button 1 = Map.delete button buttons
          decreaseButton button n = Map.insert button (n - 1) buttons
          pressButtons' useMin | length buttons == 1 || not useMin = [state'']
                               | otherwise = [state''] ++ pressButtons' False
            where (target, n) = getButton robot state buttons useMin
                  state' = pressButtons (robot + 1) (getButtonsToNavigate robot current target) True state
                  state'' = pressButtons robot (decreaseButton target n) pressA $ state' { pos = Map.insert robot target $ pos state' }

getMinLength :: String -> RobotState -> RobotState
getMinLength [] state = state
getMinLength (c:cs) state = getMinLength cs $ pressButtons 0 (Map.singleton c 1) False state

getComplexity :: String -> Int
getComplexity code = moves (getMinLength code defaultState) * read (take (length code - 1) code)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let codes = lines content
    print $ sum $ map getComplexity codes
