module Main (main) where

import Data.Char
import Data.List.Extra
import Text.Printf
import Data.Time.Clock.POSIX
import Control.Monad
import System.Clipboard
import System.Environment (getArgs, lookupEnv)
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Exit (exitFailure)
import Control.Exception
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header (hCookie)
import qualified Configuration.Dotenv as Dotenv
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as L8

import Util
import SelectYear

data Source = File | Stdin | Clipboard
data Args = ArgError String | Options { time :: Bool, year :: Int, day :: Int, part :: Int, source :: Source, extra :: [String]}

abort :: String -> IO ()
abort err = hPutStrLn stderr err >> exitFailure

parseArgs :: [String] -> Args
parseArgs ("-t":xs) = case parseArgs xs of
                           opts@(Options {}) -> opts { time = True }
                           err -> err
parseArgs [year, day, part] = parseArgs [year, day, part, "file"]
parseArgs (year:day:part:_) | not $ all (all isDigit) [year, day, part] = ArgError "Specify numbers for year, day and part."
parseArgs (_:_:_:source:_) | lower source `notElem` ["file", "stdin", "clip"] = ArgError "Specify 'file', 'stdin' or 'clip' for the source."
parseArgs (year:day:part:source:extra) = Options { time = False,
                                                   year = read year,
                                                   day = read day,
                                                   part = read part,
                                                   source = case lower source of
                                                               "file" -> File
                                                               "stdin" -> Stdin
                                                               "clip" -> Clipboard
                                                               _ -> error "Unknown source argument.",
                                                   extra }
parseArgs _ = ArgError "Illegal number of arguments."

run :: Args -> IO ()
run (ArgError err) = abort $ "Wrong arguments: " <> err
run Options {..} = do
    input <- loadInput year day source
    putStrLn $ printf "Calculating result for year %d, day %d, part %d..." year day part
    startTime <- getPOSIXTime
    case selectYear year day part $ extra ++ lines input of
         V value -> putStrLn $ printf "Result: %d" value
         Msg msg -> putStrLn msg
         Error err -> abort err
    when time $ printTimeDiff startTime

printTimeDiff :: POSIXTime -> IO ()
printTimeDiff start = do
    end <- getPOSIXTime
    let diff :: Int
        diff = round $ (end - start) * 1000
    putStrLn $ printf "\nExecution time: %02d.%02d.%03d" (diff `div` 60000) ((diff `div` 1000) `mod` 60) (diff `mod` 1000)

loadInput :: Int -> Int -> Source -> IO String
loadInput _ _ Stdin = getContents
loadInput _ _ Clipboard = do
    clipboard <- getClipboardString
    case clipboard of
         Just s -> return s
         Nothing -> do
            putStrLn "There are no textual clipboard contents."
            exitFailure
loadInput year day _ = do
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

fetchInput :: Int -> Int -> IO String
fetchInput year day = do
    catchIOError (Dotenv.loadFile Dotenv.defaultConfig) (\_ -> putStrLn "Warning: .env not found, skipping.")
    cookie <- lookupEnv "COOKIE"
    session <- case cookie of
        Just session -> return session
        Nothing -> do
            hPutStrLn stderr "COOKIE environment variable missing"
            exitFailure

    manager <- newManager tlsManagerSettings
    req <- parseRequest $ printf "https://adventofcode.com/%d/day/%d/input" year day
    let reqWithHeader = req { requestHeaders = [(hCookie, BS.pack $ "session=" <> session)] }
    response <- httpLbs reqWithHeader manager
    let body = L8.unpack $ responseBody response
    case statusCode $ responseStatus response of
        200 -> return body
        400 -> do
            hPutStrLn stderr $ printf "Server responded with 400 is your session cookie still valid? (%s)" $ trim body
            exitFailure
        code -> do
            hPutStrLn stderr $ printf "Code %d: %s" code body
            exitFailure


main :: IO ()
main = do
    args <- getArgs
    run $ parseArgs args
