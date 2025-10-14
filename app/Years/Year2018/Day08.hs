module Years.Year2018.Day08 (part1, part2) where

import Util.Util
import Data.Tuple.Extra
import Data.List.Extra
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Node = Node (Seq Node) [Int] deriving Show

parseNode :: [Int] -> (Node, [Int])
parseNode (childs:metas:xs) = first (Node nodes) $ splitAt metas rest
    where (nodes, rest) = foldr (\_ (ns, ys) -> first (ns Seq.|>) $ parseNode ys) (Seq.empty,xs) [1..childs]
parseNode _ = error "Invalid input."

sumMetadata :: Node -> Int
sumMetadata (Node childs metadata) = sum (sumMetadata <$> childs) + sum metadata

part1 :: Solution
part1 = V . sumMetadata . fst . parseNode . map read . words . hd


getValue :: Node -> Int
getValue (Node childs metadata) | null childs = sum metadata
                                | otherwise = sumOn' (maybe 0 getValue . (childs Seq.!?) . subtract 1) metadata

part2 :: Solution
part2 = V . getValue . fst . parseNode . map read . words . hd
