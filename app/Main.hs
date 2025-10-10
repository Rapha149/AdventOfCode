{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Util.Util
import Util.Args
import Util.Http
import Util.OCR
import Years.SelectYear

import Text.Printf
import Data.Time.Clock.POSIX
import Control.Monad
import System.Clipboard
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
    putStrLn $ printf "Calculating result for year %d, day %d, part %d..." year day part
    startTime <- getPOSIXTime
    answer <- handleResult $ selectYear year day part $ extra ++ lines input
    when (MeasureTime `elem` flags) $ printTimeDiff startTime
    when (AutoAnswer `elem` flags) $ autoAnswer year day part answer
    when (CheckAnswer `elem` flags && AutoAnswer `notElem` flags) $ checkAnswer year day part answer

handleResult :: Result -> IO String
handleResult = \case
    V value -> do
        putStrLn $ printf "Result: %d" value
        return $ show value
    VMsg value msg -> do
        putStrLn $ printf "Result: %d\n\n%s" value msg
        return $ show value
    RawOCR rows -> do
        let (ocr, result) = parseOCR rows
        putStrLn $ printf "OCR text:\n%s\n" ocr
        case result of
             Parsed str -> do
                 putStrLn $ printf "Result: %s" str
                 return str
             OCRError err -> do
                 abort $ printf "Error while parsing ocr: %s" err
                 return ""
    OCR grid -> let ((minX, maxX), (minY, maxY)) = getBounds $ Set.toList grid
                in handleResult $ RawOCR [[Set.member (x, y) grid | x <- [minX..maxX]] | y <- [minY..maxY]]
    Msg msg -> do
        putStrLn msg
        return msg
    Error err -> do
        abort err
        return ""

printTimeDiff :: POSIXTime -> IO ()
printTimeDiff start = do
    end <- getPOSIXTime
    let diff :: Int
        diff = round $ (end - start) * 1000
    putStrLn $ printf "\nExecution time: %02d.%02d.%03d" (diff `div` 60000) ((diff `div` 1000) `mod` 60) (diff `mod` 1000)

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
             putStrLn $ printf "Failed to read the file: \"%s\"." path
             exitFailure

main :: IO ()
main = do
    args <- getArgs
    run $ parseArgs args
