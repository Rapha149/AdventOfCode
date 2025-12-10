module Years.Year2025.Day10 (part1, part2) where

import Util.Util
import Control.Monad
import Data.Bits
import Data.Word
import Data.List.Extra
import Data.SBV

data Machine = Machine { lights :: Word16, buttons :: [Word16], joltages :: [Int] }

parseInput :: [String] -> [Machine]
parseInput = map (foldr parseMachine (Machine 0 [] []) . words)

parseMachine :: String -> Machine -> Machine
parseMachine ('[':xs) m = m { lights = bits }
    where bits = foldl setBit 0 [i | (i, '#') <- zip [0..] xs]
parseMachine ('(':xs) m = m { buttons = bits : m.buttons }
    where bits = foldl setBit 0 $ map read $ splitOn "," $ ini xs
parseMachine ('{':xs) m = m { joltages = joltages }
    where joltages = map read $ splitOn "," $ ini xs
parseMachine _ _ = error "Invalid input."

getMinLightsPresses :: Machine -> Int
getMinLightsPresses Machine{..} = minimum [length bs | bs <- subsequences buttons, foldr xor 0 bs == lights]

part1 :: Solution
part1 = V . sumOn' getMinLightsPresses . parseInput


getMinJoltagesPresses :: Machine -> IO Int
getMinJoltagesPresses Machine{..} = do
    LexicographicResult res <- optimize Lexicographic $ do
        xs <- sIntegers $ map show [0..length buttons - 1]
        mapM_ (\x -> constrain $ x .>= 0) xs
        zipWithM_ (\i j -> constrain $ sum [x | (x, b) <- zip xs buttons, testBit b i] .== fromIntegral j) [0..] joltages
        minimize "sum" $ sum xs
    case getModelValue "sum" res of
         Just n -> return $ fromInteger n
         Nothing -> error "No result."

part2 :: Solution
part2 = IOResult . (fmap (V . sum) . mapM getMinJoltagesPresses) . parseInput
