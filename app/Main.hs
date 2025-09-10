{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char
import Data.List.Extra
import Text.Printf
import Text.HTML.TagSoup
import Text.Regex
import Text.Regex.TDFA
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
import Network.HTTP.Types.Header (hCookie, hContentType)
import qualified Configuration.Dotenv as Dotenv
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as L8

import Util
import SelectYear

data Source = File | Stdin | Clipboard
data Flag = MeasureTime | AutoAnswer | CheckAnswer deriving Eq
data Args = ArgError String | Options { flags :: [Flag], year :: Int, day :: Int, part :: Int, source :: Source, extra :: [String]}

abort :: String -> IO ()
abort err = hPutStrLn stderr err >> exitFailure

parseArgs :: [String] -> Args
parseArgs (('-':flagOpt):xs) | flagOpt `notElem` ["t", "a", "ca"] = ArgError "Invalid flag argument."
                             | otherwise = let flag = case flagOpt of
                                                           "t" -> MeasureTime
                                                           "a" -> AutoAnswer
                                                           "ca" -> CheckAnswer
                                                           _ -> error "Unknown flag argument."
                                           in case parseArgs xs of
                                                   Options {..} -> Options { flags = flag : flags, .. }
                                                   err -> err
parseArgs [year, day, part] = parseArgs [year, day, part, "file"]
parseArgs (year:day:part:_) | not $ all (all isDigit) [year, day, part] = ArgError "Specify numbers for year, day and part."
parseArgs (_:_:_:source:_) | lower source `notElem` ["file", "stdin", "clip"] = ArgError "Specify 'file', 'stdin' or 'clip' for the source."
parseArgs (year:day:part:source:extra) | lower source `notElem` ["file", "stdin", "clip"] = ArgError "Invalid source argument."
                                       | otherwise = Options { flags = [],
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
    let result = selectYear year day part $ extra ++ lines input
        answer = case result of
                      V value -> show value
                      Msg msg -> msg
                      Error _ -> error "Result is an error."
    case result of
         V value -> putStrLn $ printf "Result: %d" value
         Msg msg -> putStrLn msg
         Error err -> abort err

    when (MeasureTime `elem` flags) $ printTimeDiff startTime
    when (AutoAnswer `elem` flags) $ autoAnswer year day part answer
    when (CheckAnswer `elem` flags && AutoAnswer `notElem` flags) $ checkAnswer year day part answer

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

sessionCookie :: IO String
sessionCookie = do
    catchIOError (Dotenv.loadFile Dotenv.defaultConfig) (\_ -> putStrLn "Warning: .env not found.")
    cookie <- lookupEnv "COOKIE"
    case cookie of
        Just session -> return session
        Nothing -> do
            hPutStrLn stderr "COOKIE environment variable missing."
            exitFailure

fetchInput :: Int -> Int -> IO String
fetchInput year day = do
    session <- sessionCookie
    manager <- newManager tlsManagerSettings
    initReq <- parseRequest $ printf "https://adventofcode.com/%d/day/%d/input" year day
    let req = initReq { requestHeaders = [(hCookie, BS.pack $ "session=" <> session)] }
    response <- httpLbs req manager
    let body = L8.unpack $ responseBody response
    case statusCode $ responseStatus response of
        200 -> return body
        400 -> do
            hPutStrLn stderr $ printf "[FetchInput] Server responded with 400 is your session cookie still valid? (%s)" $ trim body
            exitFailure
        code -> do
            hPutStrLn stderr $ printf "[FetchInput] Code %d: %s" code body
            exitFailure

autoAnswer :: Int -> Int -> Int -> String -> IO ()
autoAnswer year day part answer = do
    session <- sessionCookie
    manager <- newManager tlsManagerSettings
    initReq <- parseRequest $ printf "https://adventofcode.com/%d/day/%d/answer" year day
    let req = initReq { method = "POST",
                        requestHeaders = [(hCookie, BS.pack $ "session=" <> session),
                                          (hContentType, "application/x-www-form-urlencoded")],
                        requestBody = RequestBodyBS (BS.pack $ printf "level=%d&answer=%s" part answer) }
    response <- httpLbs req manager
    let body = L8.unpack $ responseBody response
    case statusCode $ responseStatus response of
        200 -> let tags = parseTags body
                   text = trim $ innerText $ takeWhile (~/= ("</main>" :: String)) $ drop 1 $ dropWhile (~/= ("<main>" :: String)) tags
                   prefix = hd $ splitOn "  " text
               in case prefix of
                       "That's the right answer!" -> putStrLn $ "\n[AutoAnswer] " <> prefix
                       "You don't seem to be solving the right level." -> checkAnswer year day part answer
                       _ -> putStrLn $ "\n[AutoAnswer] " <> unwords (words $ subRegex (mkRegex " *\\[[^]]+\\]") text "")
        400 -> do
            hPutStrLn stderr $ printf "[AutoAnswer] Server responded with 400 is your session cookie still valid? (%s)" $ trim body
            exitFailure
        code -> do
            hPutStrLn stderr $ printf "[AutoAnswer] Code %d: %s" code body
            exitFailure

checkAnswer :: Int -> Int -> Int -> String -> IO ()
checkAnswer year day part answer = do
    session <- sessionCookie
    manager <- newManager tlsManagerSettings
    initReq <- parseRequest $ printf "https://adventofcode.com/%d/day/%d" year day
    let req = initReq { requestHeaders = [(hCookie, BS.pack $ "session=" <> session)] }
    response <- httpLbs req manager
    let body = L8.unpack $ responseBody response
    case statusCode $ responseStatus response of
        200 -> let answers = map lst $ body =~ ("<p>Your puzzle answer was <code>(.+)</code>.</p>" :: String)
               in putStrLn $ "\n[CheckAnswer] " <> case (== answer) <$> answers !? (part - 1) of
                                                        Just True -> "That's the right answer!"
                                                        Just False -> "That's not the right answer."
                                                        Nothing -> "Puzzle not solved yet."
        400 -> do
            hPutStrLn stderr $ printf "[CheckAnswer] Server responded with 400 is your session cookie still valid? (%s)" $ trim body
            exitFailure
        code -> do
            hPutStrLn stderr $ printf "[CheckAnswer] Code %d: %s" code body
            exitFailure

main :: IO ()
main = do
    args <- getArgs
    run $ parseArgs args
