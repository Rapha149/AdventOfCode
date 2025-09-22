{-# LANGUAGE MultilineStrings #-}

module Util.OCR (parseOCR, OCRResult (..)) where

import Data.List.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data OCRResult = Parsed String | OCRError String String

parseLetterMap :: String -> String -> Map Char [[Bool]]
parseLetterMap letters = Map.fromList . zip letters . split (not . or) . map (map (== '#')) . transpose . lines

getLetterMap :: Int -> Map Char [[Bool]]
getLetterMap 6 = parseLetterMap "ABCEFGHIJKLOPRSUYZ" """
    .##..###...##..####.####..##..#..#.###...##.#..#.#.....##..###..###...###.#..#.#...#.####
    #..#.#..#.#..#.#....#....#..#.#..#..#.....#.#.#..#....#..#.#..#.#..#.#....#..#.#...#....#
    #..#.###..#....###..###..#....####..#.....#.##...#....#..#.#..#.#..#.#....#..#..#.#....#.
    ####.#..#.#....#....#....#.##.#..#..#.....#.#.#..#....#..#.###..###...##..#..#...#....#..
    #..#.#..#.#..#.#....#....#..#.#..#..#..#..#.#.#..#....#..#.#....#.#.....#.#..#...#...#...
    #..#.###...##..####.#.....###.#..#.###..##..#..#.####..##..#....#..#.###...##....#...####
    """
getLetterMap 10 = parseLetterMap "ABCEFGHJKLNPRXZ" """
    ..##...#####...####..######.######..####..#....#....###.#....#.#......#....#.#####..#####..#....#.######
    .#..#..#....#.#....#.#......#......#....#.#....#.....#..#...#..#......##...#.#....#.#....#.#....#......#
    #....#.#....#.#......#......#......#......#....#.....#..#..#...#......##...#.#....#.#....#..#..#.......#
    #....#.#....#.#......#......#......#......#....#.....#..#.#....#......#.#..#.#....#.#....#..#..#......#.
    #....#.#####..#......#####..#####..#......######.....#..##.....#......#.#..#.#####..#####....##......#..
    ######.#....#.#......#......#......#..###.#....#.....#..##.....#......#..#.#.#......#..#.....##.....#...
    #....#.#....#.#......#......#......#....#.#....#.....#..#.#....#......#..#.#.#......#...#...#..#...#....
    #....#.#....#.#......#......#......#....#.#....#.#...#..#..#...#......#...##.#......#...#...#..#..#.....
    #....#.#....#.#....#.#......#......#...##.#....#.#...#..#...#..#......#...##.#......#....#.#....#.#.....
    #....#.#####...####..######.#.......###.#.#....#..###...#....#.######.#....#.#......#....#.#....#.######
    """
getLetterMap _ = Map.empty

parseOCR :: [[Bool]] -> OCRResult
parseOCR rows | null letterMap = OCRError "Invalid letter height." ocr
              | otherwise = parse $ transpose rows
    where ocr = trimEnd $ unlines $ map (map (\case True -> '#'; False -> ' ')) rows
          letterMap = getLetterMap $ length rows
          parse :: [[Bool]] -> OCRResult
          parse [] = Parsed ""
          parse (col:cols) | not $ or col = parse cols
          parse cols = case Map.toList $ Map.filter (`isPrefixOf` cols) letterMap of
                            [(letter, prefix)] -> case parse $ drop (length prefix) cols of
                                                       Parsed str -> Parsed $ letter : str
                                                       err -> err
                            [] -> OCRError "No matches." ocr
                            xs -> OCRError ("Multiple matches: " <> intercalate ", " (map (singleton . fst) xs)) ocr
