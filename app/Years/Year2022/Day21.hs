module Years.Year2022.Day21 (part1, part2) where

import Util.Util
import Control.Applicative
import Data.Maybe
import Data.List.Split
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Monkey = N Int | O String String Char

parseInput :: [String] -> Map String Monkey
parseInput input = Map.fromList $ map (second (parseMonkey . words) . pair . splitOn ": ") input
    where parseMonkey :: [String] -> Monkey
          parseMonkey [d] = N $ read d
          parseMonkey [m1, op, m2] = O m1 m2 $ hd op
          parseMonkey _ = error "Invalid monkey."

getResult :: Map String Monkey -> String -> Int
getResult monkeys monkey = case monkeys Map.! monkey of
                                N n -> n
                                O m1 m2 op -> let f = case op of
                                                   '+' -> (+)
                                                   '-' -> (-)
                                                   '*' -> (*)
                                                   '/' -> div
                                                   _ -> error "Unknown operator."
                                              in getResult monkeys m1 `f` getResult monkeys m2

part1 :: Solution
part1 = V . (`getResult` "root") . parseInput


getHumanPath :: Map String Monkey -> String -> Maybe (Set String)
getHumanPath _ "humn" = Just $ Set.singleton "humn"
getHumanPath monkeys monkey = case monkeys Map.! monkey of
                                   N _ -> Nothing
                                   O m1 m2 _ -> Set.insert monkey <$> (getHumanPath monkeys m1 <|> getHumanPath monkeys m2)

getHumanValue :: Map String Monkey -> Set String -> String -> Double -> Int
getHumanValue _ _ "humn" value = round value
getHumanValue monkeys path monkey value = getHumanValue monkeys path next $ value `f` result
    where (m1, m2, op) = case monkeys Map.! monkey of
                              O m1' m2' op' -> (m1', m2', op')
                              N _ -> error "Unexpected value monkey."
          path1 = Set.member m1 path
          next = if path1 then m1 else m2
          result = fromIntegral $ getResult monkeys $ if path1 then m2 else m1
          f = case op of
                   '+' -> (-)
                   '*' -> (/)
                   '-' -> if path1 then (+) else flip (-)
                   '/' -> if path1 then (*) else flip (/)
                   _ -> error "Unknown operator."

part2 :: Solution
part2 input = let monkeys = parseInput input
                  path = fromJust $ getHumanPath monkeys "root"
                  (startHuman, startMonkey) = case monkeys Map.! "root" of
                                                   O m1 m2 _ -> if Set.member m1 path then (m1, m2) else (m2, m1)
                                                   N _ -> error "Invalid root nonkey."
              in V $ getHumanValue monkeys path startHuman $ fromIntegral $ getResult monkeys startMonkey
