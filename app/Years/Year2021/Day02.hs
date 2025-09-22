module Years.Year2021.Day02 (part1, part2) where

import Util.Util

command1 :: Vec -> String -> Vec
command1 (h, v) command = case words command of
                               ["forward", x] -> (h + read x, v)
                               ["down", x] -> (h, v + read x)
                               ["up", x] -> (h, v - read x)
                               _ -> error "Invalid command."

part1 :: Solution
part1 = V . uncurry (*) . foldl command1 (0, 0)


command2 :: (Int, Int, Int) -> String -> (Int, Int, Int)
command2 (h, v, a) command = case words command of
                                  ["forward", x] -> (h + read x, v + a * read x, a)
                                  ["down", x] -> (h, v, a + read x)
                                  ["up", x] -> (h, v, a - read x)
                                  _ -> error "Invalid command."

part2 :: Solution
part2 = V . (\(h,v,_) -> h * v) . foldl command2 (0, 0, 0)
