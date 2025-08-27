module Year2022.Day19 (part1, part2) where

import Util
import Data.Bits
import Data.Word
import Data.Char
import Data.Ord
import Data.List.Extra
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Resource = Ore | Clay | Obs | Geo deriving (Eq, Ord, Enum, Show)
type Robot = Resource
type Blueprint = Map Resource Word64

toR :: Word16 -> Resource -> Word64
toR amount r = fromIntegral amount `shiftL` (fromEnum r * 16)

getR :: Word64 -> Resource -> Word16
getR resources r = fromIntegral $ resources `shiftR` (fromEnum r * 16)

parseBlueprint :: String -> Blueprint
parseBlueprint = Map.fromList . zip (enumFrom Ore) . map (parseResources . words) . splitOn ". "
    where parseResources :: [String] -> Word64
          parseResources [] = 0
          parseResources [_] = 0
          parseResources (n:x:xs) | all isDigit n = let r = case if lst x == '.' then ini x else x of
                                                                 "ore" -> Ore
                                                                 "clay" -> Clay
                                                                 "obsidian" -> Obs
                                                                 _ -> error "Invalid blueprint resource."
                                                    in toR (read n) r + parseResources xs
                                  | otherwise = parseResources (x:xs)

getMaxGeodes :: Int -> Int -> Blueprint -> Word64 -> [(Word64, Word64)] -> Int -> Word64 -> Word64
getMaxGeodes _ _ _ _ _ (-1) maxValue = maxValue
getMaxGeodes beam minutes blueprint maxNeeded states timeLeft maxValue = let states' = concatMap getNextStates states
                                                                             maxValue' = max maxValue $ maximum $ map fst states'
                                                                             scores = map (score &&& id) states'
                                                                             bestScores = map snd $ take ((minutes - timeLeft) `shiftL` beam) $ sortOn (Down . fst) scores
                                                                         in getMaxGeodes beam minutes blueprint maxNeeded bestScores (timeLeft - 1) maxValue'
    where getNextStates :: (Word64, Word64) -> [(Word64, Word64)]
          getNextStates (resources, robots) = let types = filter (\t -> hasTime t && isEnough resources t && isNeeded robots t) $ enumFrom Ore
                                              in (resources + robots, robots) : map (\t -> (resources + robots - blueprint Map.! t, robots + toR 1 t)) types
          hasTime :: Robot -> Bool
          hasTime Ore = timeLeft >= 15
          hasTime Clay = timeLeft >= 6
          hasTime Obs = timeLeft >= 3
          hasTime Geo = timeLeft >= 1
          isEnough :: Word64 -> Robot -> Bool
          isEnough resources robot = let needed = blueprint Map.! robot
                           in all (uncurry (>=) . (getR resources &&& getR needed)) $ enumFrom Ore
          isNeeded :: Word64 -> Robot -> Bool
          isNeeded robots robot = getR robots robot < getR maxNeeded robot
          score :: (Word64, Word64) -> Word64
          score (resources, robots) = let t16 = fromIntegral timeLeft -- if you had infinite resources, how many geodes are possible with the time left
                                          t64 = fromIntegral timeLeft -- existing resources + timeLeft times robots + 1 more geode every minute (1+2+3+...)
                                      in resources + t64 * robots + toR (t16 * (t16 + 1) `div` 2) Geo

lineToMaxGeodes :: Int -> Int -> String -> Int
lineToMaxGeodes beam minutes line = fromIntegral $ getR maxValue Geo
    where blueprint = parseBlueprint line
          maxNeeded = toR 0xFFFF Geo .|. sumOn' (\r -> toR (Map.foldr (\x -> max $ getR x r) 0 blueprint) r) (enumFrom Ore)
          maxValue = getMaxGeodes beam minutes blueprint maxNeeded [(0, toR 1 Ore)] (minutes - 1) 0

part1 :: Solution
part1 = V . sum . zipWith (*) [1..] . map (lineToMaxGeodes 2 24)

part2 :: Solution
part2 = V . productOn' (lineToMaxGeodes 7 32) . take 3
