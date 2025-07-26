module Main (main) where

import System.Environment (getArgs, lookupEnv)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Clipboard
import Control.Exception
import qualified Configuration.Dotenv as Dotenv
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header (hCookie)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import Data.List.Extra
import Text.Printf

import SelectYear

data Source = File | Stdin | Clipboard
data Args = ArgError String | Options Int Int Int Source

abort :: String -> IO ()
abort err = hPutStrLn stderr err >> exitFailure

parseArgs :: [String] -> Args
parseArgs [year, day, part] = parseArgs [year, day, part, "file"]
parseArgs [year, day, part, source] | all (all isDigit) [year, day, part] = Options (read year) (read day) (read part) (case lower source of
        "stdin" -> Stdin
        "clip" -> Clipboard
        _ -> File)
parseArgs _ = ArgError "Illegal number of arguments."

start :: Args -> IO ()
start (ArgError err) = abort $ "Wrong arguments: " <> err
start (Options year day part stdin) = do
    input <- loadInput year day stdin
    putStrLn $ "\n" <> show (selectYear year day part (lines input))

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
    start $ parseArgs args
