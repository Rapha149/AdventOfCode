module Years.Year2019.Day17 (part1, part2) where

import Util.Util
import Years.Year2019.IntcodeComputer
import Data.Char
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM

getGrid :: [String] -> ((Vec, Vec), Set Vec)
getGrid input = ((robot, delta), Set.fromList [(x, y) | (y, row) <- zip [0..] $ split (== 10) outputs, (x, n) <- zip [0..] row, n /= 46])
    where State {outputs} = run $ parseState input
          grid = Map.fromList [((x, y), c) | (y, row) <- zip [0..] $ lines $ map chr outputs, (x, c) <- zip [0..] row]
          (robot, dir) = Map.findMin $ Map.filter (`elem` "^>v<") grid
          delta = case dir of
                       '^' -> (0, -1)
                       '>' -> (1, 0)
                       'v' -> (0, 1)
                       '<' -> (-1, 0)
                       _ -> error "Invalid direction."

part1 :: Solution
part1 input = V $ sumOn' (uncurry (*)) intersections
    where (_, scaffold) = getGrid input
          intersections = filter (\(x, y) -> all (`Set.member` scaffold) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]) $ Set.toList scaffold


getPath :: Set Vec -> Vec -> Vec -> Set Vec -> [(Char, Int)]
getPath scaffold pos (dx, dy) seen = case filter ((`Set.member` scaffold) . onBoth (+) pos . snd) [('L', (dy, -dx)), ('R', (-dy, dx))] of
                                          [] -> if scaffold == seen then [] else error "No path found."
                                          [(dir, delta)] -> let path = goStraight delta pos
                                                            in (dir, length path) : getPath scaffold (lst path) delta (Set.union seen $ Set.fromList path)
                                          _ -> error "Path forks."
    where goStraight :: Vec -> Vec -> [Vec]
          goStraight delta p = let p' = onBoth (+) p delta
                               in if Set.member p' scaffold then p' : goStraight delta p' else []

getFunctions :: [(Char, Int)] -> [[(Char, Int)]]
getFunctions [] = []
getFunctions (('X', 0):path) = getFunctions path
getFunctions path = function : getFunctions (replaceSubseqs path)
    where function = maximumOn length [prefix | (prefix, suffix) <- zip (inits path) (tails path),
                                                not $ null prefix, ('X', 0) `notElem` prefix,
                                                sumOn' ((+ 2) . length . show . snd) prefix + length prefix - 1 <= 20,
                                                prefix `isInfixOf` suffix]
          replaceSubseqs :: [(Char, Int)] -> [(Char, Int)]
          replaceSubseqs [] = []
          replaceSubseqs (x:xs) | function `isPrefixOf` (x:xs) = ('X', 0) : replaceSubseqs (drop (length function - 1) xs)
                                | otherwise = x : replaceSubseqs xs

getMainRoutine :: [(Char, [(Char, Int)])] -> [(Char, Int)] -> [Char]
getMainRoutine _ [] = []
getMainRoutine fs path = c : getMainRoutine fs (drop (length f) path)
    where (c, f) = hd $ filter ((`isPrefixOf` path) . snd) fs

part2 :: Solution
part2 input = V $ lst $ outputs $ run $ state { program = IM.insert 0 2 state.program }
    where ((pos, dir), scaffold) = getGrid input
          path = getPath scaffold pos dir (Set.singleton pos)
          functions = getFunctions path
          mainRoutine = getMainRoutine (zip ['A'..] functions) path
          inputs = map ord $ unlines $ intercalate "," (map singleton mainRoutine) :
                                       map (intercalate "," . map (\(c, n) -> [c] ++ "," ++ show n)) functions ++
                                       ["n"]
          state = parseStateI inputs input
