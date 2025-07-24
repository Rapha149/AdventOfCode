module Main where
import Text.Regex.TDFA

maxX = 101 :: Int
maxY = 103 :: Int
middleX = maxX `div` 2
middleY = maxY `div` 2

parseContent :: [String] -> [((Int, Int), (Int, Int))]
parseContent [] = []
parseContent (l:ls) = ((x, y), (vx, vy)) : parseContent ls
    where (_, _, _, groups) = l =~ "^p=([0-9]+),([0-9]+) v=(-?[0-9]+),(-?[0-9]+)$" :: (String, String, String, [String])
          [x, y, vx, vy] = map read groups

move :: ((Int, Int), (Int, Int)) -> Int -> (Int, Int)
move (pos, _) 0 = pos
move ((x, y), (vx, vy)) n = move (((x + vx) `mod` maxX, (y + vy) `mod` maxY), (vx, vy)) $ n - 1

getQuadrant :: (Int, Int) -> Maybe Int
getQuadrant (x, y) | x == middleX || y == middleY = Nothing
                   | otherwise = Just $ (fromEnum $ x > middleX) + (fromEnum $ y > middleY) * 2

main :: IO ()
main = do
    content <- readFile "input.txt"
    let robots = parseContent $ lines content
        moved = map (\r -> move r 100) robots
        quadrants = map getQuadrant moved
    print $ product $ map (\n -> length $ filter (== Just n) quadrants) [0 .. 3]
