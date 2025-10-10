module Years.Year2019.Day25 (part1) where

import Util.Util
import Years.Year2019.IntcodeComputer
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Regex.TDFA

data Output = Output { room :: String, doors :: [String], items :: [String]} deriving Show

invDir :: String -> String
invDir "north" = "south"
invDir "east" = "west"
invDir "south" = "north"
invDir "west" = "east"
invDir _ = error "Invalid direction."

parseOutput :: State -> Maybe Output
parseOutput State {outputs} | lst output /= "Command?" = Nothing
                            | not $ "== " `isPrefixOf` hd output = Just $ Output (hd output) [] []
                            | otherwise = Just $ Output {..}
    where output = lines $ trim $ map chr outputs
          room = dropEnd 3 $ drop 3 $ hd output
          parts = split null output
          doors | "Doors here lead:" `elem` output = map (drop 2) $ tl $ parts !! 1
                | otherwise = []
          items | "Items here:" `elem` output = map (drop 2) $ tl $ parts !! 2
                | otherwise = []

command :: State -> String -> State
command state cmd = run $ state { inputs = map ord cmd ++ [10], outputs = [] }

testItem :: State -> String -> Bool
testItem _ "infinite loop" = False
testItem state item = isJust (parseOutput state1) && case parseOutput state2 of
                                                          Nothing -> False
                                                          Just (Output {room}) -> "You drop the " `isPrefixOf` room
    where state1 = command state $ "take " ++ item
          state2 = command state1 $ "drop " ++ item

explore :: State -> Map String [String] -> [String] -> Map String ([String], [String])
explore state triedWays path | room == "Security Checkpoint" = Map.insert room (validItems, reverse path) $ explore (command state $ invDir $ hd path) triedWays (tl path)
                             | Map.notMember room triedWays = (if null validItems then id else Map.insert room (validItems, reverse path)) $
                                                                explore state (Map.insert room (map invDir $ take 1 path) triedWays) path
                             | null tryable = if null path then Map.empty else explore (command state $ invDir $ hd path) triedWays (tl path)
                             | otherwise = explore (command state door) (Map.adjust (door :) room triedWays) (door : path)
    where Output {..} = fromJust $ parseOutput state
          validItems = filter (testItem state) items
          tried = triedWays Map.! room
          tryable = doors \\ tried
          door = hd tryable

collectItems :: State -> ([String], [String]) -> State
collectItems state (items, path) = back
    where arrived = foldl command state path
          took = foldl command arrived $ map ("take " ++) items
          back = foldl command took $ map invDir $ reverse path

tryItems :: State -> String -> [String] -> Maybe Int
tryItems state door items = listToMaybe $ map read groups
    where State {outputs} = command (foldl command state $ map ("take " ++) items) door
          groups :: [String] = drop 1 $ getAllTextSubmatches $ map chr outputs =~ "typing ([0-9]+) on the keypad"

controlRobot :: String -> State -> String
controlRobot input state = map chr state'.outputs ++ controlRobot (tl input) (state' { inputs = [ord $ hd input], outputs = [] })
    where state' = run state

part1 :: Solution
part1 raw | play = Msg $ controlRobot (unlines $ tl input) $ parseState [hd input]
          | otherwise = case mapMaybe (tryItems dropItems securityDoor) $ subsequences items of
                             (num:_) -> V num
                             [] -> Error "No item combination matches the weight requirement."
    where (play, input) = getExtra1 (== "play") (const True) False raw
          state = run $ parseState input
          rooms = explore state Map.empty []
          items = concatMap fst rooms
          (_, securityPath) = rooms Map.! "Security Checkpoint"
          security = foldl command (foldl collectItems state rooms) securityPath
          securityDoor = case delete (invDir $ lst securityPath) $ doors $ fromJust $ parseOutput security of
                              [door] -> door
                              _ -> error "None or multiple door possibilities for the pressure-sensitive floor."
          dropItems = foldl command security $ map ("drop " ++) items
