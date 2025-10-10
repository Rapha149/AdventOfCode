module Years.Year2019.Day13 (part1, part2) where

import Util.Util
import Years.Year2019.IntcodeComputer
import Data.List.Extra
import qualified Data.IntMap.Strict as IM

part1 :: Solution
part1 input = V $ sum [1 | [_, _, 2] <- chunksOf 3 (outputs $ run $ parseState input)]


data Game = Game { ball :: Int, paddle :: Int, points :: Int }

play :: Game -> State -> Int
play game state | state'.status == Finished = points
                | otherwise = play game' $ state' { inputs = [signum $ ball - paddle], outputs = [] }
    where state' = run state
          game'@Game {..} = handleOutputs state'.outputs game
          handleOutputs :: [Int] -> Game -> Game
          handleOutputs (-1:0:p:is) g = handleOutputs is $ g { points = p }
          handleOutputs (x:_:t:is) g = handleOutputs is $ case t of
                                                               3 -> g { paddle = x }
                                                               4 -> g { ball = x }
                                                               _ -> g
          handleOutputs _ g = g

part2 :: Solution
part2 = V . play (Game 0 0 0) . (\s -> s { program = IM.insert 0 2 s.program }) . parseState
