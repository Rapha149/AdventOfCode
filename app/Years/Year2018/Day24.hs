module Years.Year2018.Day24 (part1, part2) where

import Util.Util
import Data.Ord
import Data.List.Extra
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Regex.PCRE

data Army = ImmuneSystem | Infection deriving (Show, Eq, Ord)
type GroupId = (Army, Int)
data Group = Group { units :: Int, hitPoints :: Int, weaknesses :: [String], immunities :: [String],
                     damage :: Int, damageType :: String, initiative :: Int } deriving (Show, Eq)

regex, weakRegex, immuneRegex :: String
regex = "^(\\d+) units each with (\\d+) hit points (?:\\([a-z,; ]+\\) )?with an attack that does (\\d+) ([a-z]+) damage at initiative (\\d+)$"
weakRegex = "weak to [a-z, ]+"
immuneRegex = "immune to [a-z, ]+"

parseInput :: Maybe Army -> Int -> [String] -> Map GroupId Group
parseInput _ _ [] = Map.empty
parseInput _ _ ("Immune System:":ls) = parseInput (Just ImmuneSystem) 0 ls
parseInput _ _ ("Infection:":ls) = parseInput (Just Infection) 0 ls
parseInput army idx ("":ls) = parseInput army idx ls
parseInput (Just army) idx (l:ls) = Map.insert (army, idx) Group{..} $ parseInput (Just army) (idx + 1) ls
    where (units, hitPoints, damage, damageType, initiative) = case getAllTextSubmatches $ l =~ regex of
                [_, u, hp, d, dt, i] -> (read u, read hp, read d, dt, read i)
                _ -> error "Invalid input."
          weaknesses = case l =~ weakRegex of
                            "" -> []
                            s -> splitOn ", " $ drop 8 s
          immunities = case l =~ immuneRegex of
                            "" -> []
                            s -> splitOn ", " $ drop 10 s
parseInput Nothing _ _ = error "Input is missing splitting lines."

effectivePower :: Group -> Int
effectivePower Group{..} = units * damage

targetSelection :: Map GroupId Group -> Map GroupId GroupId
targetSelection groups = getTargets groups $ map fst $ sortOn (\(_, g) -> Down (effectivePower g, g.initiative)) $ Map.toList groups
    where getTargets :: Map GroupId Group -> [GroupId] -> Map GroupId GroupId
          getTargets _ [] = Map.empty
          getTargets selectable (groupId@(army, _):xs) = let g@Group{damageType} = groups Map.! groupId
                                                             power = effectivePower g
                                                         in case Map.toList $ Map.filterWithKey (\(a, _) t -> army /= a && damageType `notElem` t.immunities) selectable of
                                                                 [] -> getTargets selectable xs
                                                                 ts -> let (targetId, _) = maximumOn (\(_, t) -> (power * if damageType `elem` t.weaknesses then 2 else 1, effectivePower t, t.initiative)) ts
                                                                       in Map.insert groupId targetId $ getTargets (Map.delete targetId selectable) xs

attacking :: Map GroupId Group -> Map GroupId GroupId -> Map GroupId Group
attacking groups targets = foldl attack groups $ sortOn (Down . initiative . (groups Map.!)) $ Map.keys targets
    where attack :: Map GroupId Group -> GroupId -> Map GroupId Group
          attack grps groupId | groupId `Map.notMember` grps = grps
                              | otherwise = let targetId = targets Map.! groupId
                                                g = grps Map.! groupId
                                                t = grps Map.! targetId
                                                damage = effectivePower g * if g.damageType `elem` t.weaknesses then 2 else 1
                                                dead = damage `div` t.hitPoints
                                                remaining = t.units - dead
                                            in if remaining >= 0 then Map.insert targetId (t { units = remaining }) grps
                                                                 else Map.delete targetId grps

fight :: Map GroupId Group -> (Maybe Army, Map GroupId Group)
fight groups = case Set.toList $ Set.map fst $ Map.keysSet groups of
                    [] -> error "No army left."
                    [army] -> (Just army, groups)
                    _ | groups == groups' -> (Nothing, groups')
                      | otherwise -> fight groups'
    where targets = targetSelection groups
          groups' = attacking groups targets

part1 :: Solution
part1 = V . sum . Map.map units . snd . fight . parseInput Nothing 0

part2 :: Solution
part2 input = V $ hd [sum $ Map.map units winners | boost <- [1..], let boosted = Map.mapWithKey (\(army, _) g -> if army == ImmuneSystem then g { damage = g.damage + boost } else g) groups,
                                                    (Just ImmuneSystem, winners) <- [fight boosted]]
    where groups = parseInput Nothing 0 input
