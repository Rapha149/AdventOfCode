module Year2022.Day07 (part1, part2) where

import Util
import Data.List
import Data.List.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Path = [String]
data File = F Int | D String deriving Show

parseCmd :: (Path, Map Path [File]) -> String -> (Path, Map Path [File])
parseCmd (cwd, files) ('c':'d':' ':d) = case d of
                                             "/" -> ([], files)
                                             ".." -> (tl cwd, files)
                                             _ -> (d:cwd, files)
parseCmd (cwd, files) ('l':'s':'\n':fs) = (cwd, foldr (Map.insertWith (++) cwd . singleton . parseFile) files $ lines fs)
    where parseFile :: String -> File
          parseFile ('d':'i':'r':' ':d) = D d
          parseFile f = F $ read $ hd $ words f
parseCmd _ _ = error "Invalid command."

getSize :: Map Path [File] -> Path -> Map Path Int -> Map Path Int
getSize files path sizes = Map.insert path (sumOn' size $ files Map.! path) sizes
    where size :: File -> Int
          size (F s) = s
          size (D d) = sizes Map.! (d:path)

getSizes :: [String] -> [Int]
getSizes input = Map.elems $ foldr (getSize files) Map.empty $ sortOn length $ Map.keys files
    where (_, files) = foldl parseCmd ([], Map.empty) $ splitOn "\n$ " $ drop 2 $ intercalate "\n" input

part1 :: Solution
part1 = V . sum . filter (<= 100000) . getSizes

part2 :: Solution
part2 input = let sizes = getSizes input
                  minSize = maximum sizes - 40000000
              in V $ minimum $ filter (>= minSize) sizes
