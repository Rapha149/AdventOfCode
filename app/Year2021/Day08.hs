module Year2021.Day08 (part1, part2) where

import Util
import Data.Bits
import Data.Word
import Data.Char
import Data.List.Extra
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Bimap  (Bimap)
import qualified Data.Bimap as BM

part1 :: Solution
part1 = V . sumOn' (length . filter ((`elem` [2,3,4,7]) . length) . words . lst . splitOn " | ")


segments :: Bimap Int Word8
segments = BM.fromList [(0, 0b1110111), (1, 0b0010010), (2, 0b1011101), (3, 0b1011011), (4, 0b0111010),
                        (5, 0b1101011), (6, 0b1101111), (7, 0b1010010), (8, 0b1111111), (9, 0b1111011)]

segmentLengths :: Map Int Int
segmentLengths = Map.map popCount $ BM.toMap segments

getCombinations :: Int -> Word8 -> [Word8]
getCombinations n bits = [bits .&. foldl setBit 0 bs | bs <- subsequences $ filter (testBit bits) [0..7], length bs == n]

parseLayout :: [Int] -> [String] -> Int -> Map Char Word8 -> Map Char Word8
parseLayout numbers patterns idx layout | all ((== 1) . popCount) $ Map.elems layout = layout
                                        | null numbers || null patterns = error "No numbers/patterns left."
                                        | idx >= length patterns = parseLayout numbers patterns 0 layout
                                        | length matching > 1 = parseLayout numbers patterns (idx + 1) layout
                                        | null matching = error "No matching numbers."
                                        | otherwise = let num = hd matching
                                                          segment = segments BM.! num
                                                          mappings = Map.map (\ms -> case filter (\m -> segment .&. m == m) ms of
                                                                                          [m] -> m
                                                                                          _ -> error "None or multiple matching combinations.") combinations
                                                          newLayout = Map.foldrWithKey (\chars mask acc -> foldr (`Map.insert` mask) (Map.map (.&. complement mask) acc) chars) layout mappings
                                                      in parseLayout (delete num numbers) (delete p patterns) idx newLayout
    where p = patterns !! idx
          combinations = Map.foldrWithKey (\bits chars -> Map.insert chars $ getCombinations (length chars) bits) Map.empty $
                         Map.foldrWithKey (\k v -> Map.insertWith (++) v [k]) Map.empty $ Map.restrictKeys layout $ Set.fromList p
          masks = map sum $ sequence $ Map.elems combinations
          matching = filter (\x -> segmentLengths Map.! x == length p && segments BM.! x `elem` masks) numbers

getOutputValue :: String -> Int
getOutputValue line = read $ map (intToDigit . getValue) outDigits
    where (patterns, outDigits) = tuple $ map words $ splitOn " | " line
          layout = parseLayout [0..9] patterns 0 (Map.fromList $ map (, 0b1111111) ['a'..'g'])
          getValue :: String -> Int
          getValue digit = segments BM.!> Map.foldr (+) 0 (Map.restrictKeys layout $ Set.fromList digit)

part2 :: Solution
part2 = V . sumOn' getOutputValue
