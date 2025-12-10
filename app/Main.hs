{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Util.Util
import Util.Args
import Util.Http
import Util.OCR
import Years.SelectYear

import Text.Printf
import Data.List.Extra
import Data.Tuple.Extra
import Data.Time.Clock.POSIX
import Control.Monad
import System.Clipboard
import Control.DeepSeq
import System.Environment
import System.Directory
import System.IO
import System.IO.Error
import System.Exit
import System.Process
import Control.Exception
import qualified Data.Set as Set

abort :: String -> IO ()
abort err = hPutStrLn stderr err >> exitFailure

run :: Args -> IO ()
run (ArgError err) = abort $ "Wrong arguments: " <> err
run Options {..} = do
    input <- getInput year day source
    when (null $ trim input) $ abort "The input is empty."

    let runOnce silent = do
            input' <- evaluate $ force $ extra ++ dropWhileEnd null (lines input)
            start <- getPOSIXTime
            result <- handleResult silent $ selectYear year day part input'
            result `deepseq` do
                end <- getPOSIXTime
                return (result, round $ (end - start) * 1000000 :: Int)

    unless (NoResult `elem` flags) $ do
        printf "Calculating result for year %d, day %d, part %d...\n" year day part
        (answer, diff) <- runOnce False
        when (MeasureTime `elem` flags) $ printf "\nExecution time: %s\n" $ formatTime diff
        when (AutoAnswer `elem` flags) $ autoAnswer year day part answer
        when (CheckAnswer `elem` flags && AutoAnswer `notElem` flags) $ checkAnswer year day part answer

    case avgRuns of
         Just n | n > 0 -> do
             unless (NoResult `elem` flags) $ putStrLn ""
             putStrLn "Calculating average execution time..."
             (times, maxLength) <- foldM (\(acc, maxLength) i -> do
                    time <- snd <$> runOnce True
                    let acc' = time : acc
                        avg = sum acc' `div` i
                        line = printf "Run %d/%d: %s (current average: %s)" i n (formatTime time) (formatTime avg)
                    putStr $ '\r' : line
                    hFlush stdout
                    return (acc', max maxLength $ length line)
                 ) ([], 0) [1..n]
             let avg = sum times `div` n
                 sorted = sort times
                 median = (sorted !! (n `div` 2) + sorted !! ((n + 1) `div` 2)) `div` 2
                 (mn, mx) = (hd &&& lst) sorted
                 line :: String = printf "Execution time of %d runs:" n
             printf "\r%s%s\n" line $ replicate (maxLength - length line) ' '
             printf "    average: %s\n    median:  %s\n    min:     %s\n    max:     %s\n" (formatTime avg) (formatTime median) (formatTime mn) (formatTime mx)
         _ -> return ()

formatTime :: Int -> String
formatTime micros | seconds > 0 = printf "%02d:%02d.%03d" minutes (seconds `mod` 60) (millis `mod` 1000)
                  | otherwise = printf "%d.%03d ms" millis (micros `mod` 1000)
    where millis = micros `div` 1000
          seconds = millis `div` 1000
          minutes = seconds `div` 60

handleResult :: Bool -> Result -> IO String
handleResult silent = \case
    V value -> do
        unless silent $ printf "Result: %d\n" value
        return $ show value
    VMsg value msg -> do
        unless silent $ printf "Result: %d\n\n%s\n" value msg
        return $ show value
    RawOCR rows -> do
        let (ocr, result) = parseOCR rows
        unless silent $ printf "OCR text:\n%s\n\n" ocr
        case result of
             Parsed str -> do
                 unless silent $ printf "Result: %s\n" str
                 return str
             OCRError err -> do
                 abort $ printf "Error while parsing ocr: %s" err
                 return ""
    OCR grid -> let ((minX, maxX), (minY, maxY)) = getBounds $ Set.toList grid
                in handleResult silent $ RawOCR [[Set.member (x, y) grid | x <- [minX..maxX]] | y <- [minY..maxY]]
    Msg msg -> do
        unless silent $ putStrLn msg
        return msg
    Error err -> do
        abort err
        return ""
    IOResult ioRes -> do
        res <- ioRes
        handleResult silent res

getInput :: Int -> Int -> Source -> IO String
getInput year day File = loadFileInput year day
getInput _ _ Stdin = getContents
getInput year day FileStdin = do
    inputFile <- loadFileInput year day
    inputStdin <- getContents
    return $ inputFile ++ inputStdin
getInput _ _ Clipboard = do
    clipboard <- getClipboardString
    case clipboard of
         Just s -> return s
         Nothing -> do
            putStrLn "There are no textual clipboard contents."
            exitFailure
getInput _ _ WaylandClipboard = do
    (code, clipboard, _) <- readProcessWithExitCode "wl-paste" [] ""
    case code of
         ExitSuccess -> return clipboard
         ExitFailure _ -> do
            putStrLn "There are no textual clipboard contents."
            exitFailure

loadFileInput :: Int -> Int -> IO String
loadFileInput year day = do
    let dirPath = printf "./inputs/%d" year
        path = printf "%s/%02d.txt" dirPath day
    result <- try (readFile path) :: IO (Either IOError String)
    case result of
         Right contents -> return contents
         Left e | isDoesNotExistError e -> do
             input <- fetchInput year day
             createDirectoryIfMissing True dirPath
             writeFile path input
             pure input
         Left _ -> do
             printf "Failed to read the file: \"%s\".\n" path
             exitFailure

main :: IO ()
main = do
    args <- getArgs
    run $ parseArgs args
