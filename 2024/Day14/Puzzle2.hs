module Main where
import qualified Data.Set as Set
import Text.Regex.TDFA
import Codec.Picture
import Codec.Picture.Types

maxX = 101 :: Int
maxY = 103 :: Int
middleX = maxX `div` 2
middleY = maxY `div` 2

parseContent :: [String] -> [((Int, Int), (Int, Int))]
parseContent [] = []
parseContent (l:ls) = ((x, y), (vx, vy)) : parseContent ls
    where (_, _, _, groups) = l =~ "^p=([0-9]+),([0-9]+) v=(-?[0-9]+),(-?[0-9]+)$" :: (String, String, String, [String])
          [x, y, vx, vy] = map read groups

move :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
move ((x, y), (vx, vy)) = (((x + vx) `mod` maxX, (y + vy) `mod` maxY), (vx, vy))

pixelColor :: Set.Set (Int, Int) -> Int -> Int -> Pixel8
pixelColor robots x y | Set.member (x, y) robots = 255
                      | otherwise = 0

createImages :: [((Int, Int), (Int, Int))] -> Int -> IO ()
createImages _ 10000 = return ()
createImages robots n = do
    let robotSet = Set.fromList $ map fst robots
        image = generateImage (\x y -> pixelColor robotSet x y) maxX maxY
    savePngImage ("images/" ++ show n ++ ".png") $ ImageY8 image
    print n
    createImages (map move robots) (n + 1)


main :: IO ()
main = do
    content <- readFile "input.txt"
    let robots = parseContent $ lines content
    createImages robots 0
