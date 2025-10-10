module Years.Year2024.Day23 (part1, part2) where

import Util.Util
import Data.List
import Data.List.Extra
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set

parseInput :: [String] -> ([String], Set (String, String))
parseInput input = (Set.toList $ Set.map fst connections, connections)
    where connections = Set.fromList $ concatMap ((\x -> [x, swap x]) . pair . splitOn "-") input

part1 :: Solution
part1 input = V $ sum [1 | (a:x1) <- tails computers, (b:x2) <- tails x1, Set.member (a, b) connections,
                           c <- x2, Set.member (a, c) connections, Set.member (b, c) connections,
                           hd a == 't' || hd b == 't' || hd c == 't']
    where (computers, connections) = parseInput input

part2 :: Solution
part2 input = Msg $ intercalate "," $ maximumOn length [a:bs | (a:xs) <- tails computers,
                                                               bs <- subsequences $ filter ((`Set.member` connections) . (a,)) xs,
                                                               all (`Set.member` connections) [(c, d) | (c:ys) <- tails bs, d <- ys]]
    where (computers, connections) = parseInput input
