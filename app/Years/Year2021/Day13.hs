module Years.Year2021.Day13 (part1, part2) where

import Util.Util
import Data.List.Extra
import Data.Tuple.Extra
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> (Set Vec, [Vec])
parseInput input = (dots, folds)
    where [inputDots, inputFolds] = split null input
          dots = Set.fromList $ map (pair . map read . splitOn ",") inputDots
          folds = map (parseFold . (!! 2) . words) inputFolds
          parseFold :: String -> Vec
          parseFold ('y':'=':y) = (0, read y)
          parseFold ('x':'=':x) = (read x, 0)
          parseFold _ = error "Invalid fold instruction."

fold :: Set Vec -> Vec -> Set Vec
fold dots (x, y) = Set.union dots1 $ Set.map (change (half * 2 -)) dots2
    where (sel, change) = if y == 0 then (fst, first) else (snd, second)
          half = sel (x, y)
          (dots1, dots2) = Set.partition ((< half) . sel) $ Set.filter ((/= half) . sel) dots

part1 :: Solution
part1 = V . Set.size . uncurry fold . second hd . parseInput

part2 :: Solution
part2 = OCR . uncurry (foldl fold) . parseInput
