{-# LANGUAGE OverloadedStrings #-}

module Util.Http (fetchInput, autoAnswer, checkAnswer) where

import Util.Util
import Data.List.Extra
import Text.Printf
import Text.HTML.TagSoup
import Text.Regex
import Text.Regex.TDFA
import System.Environment
import System.IO
import System.IO.Error
import System.Exit
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Configuration.Dotenv as Dotenv
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as L8

getEnvVar :: String -> IO String
getEnvVar var = do
    catchIOError (Dotenv.loadFile Dotenv.defaultConfig) (\_ -> putStrLn "Warning: .env not found.")
    cookie <- lookupEnv var
    case cookie of
        Just session -> return session
        Nothing -> do
            hPutStrLn stderr $ var <> " environment variable missing."
            exitFailure

getHttpResponse :: Request -> IO (Int, String)
getHttpResponse initReq = do
    session <- getEnvVar "SESSION_COOKIE"
    userAgent <- getEnvVar "USER_AGENT"
    let req = initReq { requestHeaders = requestHeaders initReq <> [(hCookie, BS.pack $ "session=" <> session),
                                                                    (hUserAgent, BS.pack userAgent)] }
    manager <- newManager tlsManagerSettings
    response <- httpLbs req manager
    return (statusCode $ responseStatus response, L8.unpack $ responseBody response)

fetchInput :: Int -> Int -> IO String
fetchInput year day = do
    req <- parseRequest $ printf "https://adventofcode.com/%d/day/%d/input" year day
    (code, body) <- getHttpResponse req
    case code of
        200 -> return body
        400 -> do
            hPutStrLn stderr $ printf "[FetchInput] Server responded with 400 is your session cookie still valid? (%s)" $ trim body
            exitFailure
        _ -> do
            hPutStrLn stderr $ printf "[FetchInput] Code %d: %s" code body
            exitFailure

autoAnswer :: Int -> Int -> Int -> String -> IO ()
autoAnswer year day part answer = do
    req <- parseRequest $ printf "https://adventofcode.com/%d/day/%d/answer" year day
    (code, body) <- getHttpResponse $ req { method = "POST",
                                            requestHeaders = [(hContentType, "application/x-www-form-urlencoded")],
                                            requestBody = RequestBodyBS (BS.pack $ printf "level=%d&answer=%s" part answer) }
    case code of
        200 -> let tags = parseTags body
                   text = trim $ innerText $ takeWhile (~/= ("</main>" :: String)) $ drop 1 $ dropWhile (~/= ("<main>" :: String)) tags
                   prefix = hd $ splitOn "  " text
               in case prefix of
                       "That's the right answer!" -> putStrLn $ "\n[AutoAnswer] " <> prefix
                       "You don't seem to be solving the right level." -> checkAnswer year day part answer
                       _ -> do
                           putStrLn $ "\n[AutoAnswer] " <> unwords (words $ subRegex (mkRegex " *\\[[^]]+\\]") text "")
                           exitFailure
        400 -> do
            hPutStrLn stderr $ printf "[AutoAnswer] Server responded with 400 is your session cookie still valid? (%s)" $ trim body
            exitFailure
        _ -> do
            hPutStrLn stderr $ printf "[AutoAnswer] Code %d: %s" code body
            exitFailure

checkAnswer :: Int -> Int -> Int -> String -> IO ()
checkAnswer year day part answer = do
    req <- parseRequest $ printf "https://adventofcode.com/%d/day/%d" year day
    (code, body) <- getHttpResponse req
    case code of
        200 -> let answers = map lst $ body =~ ("<p>Your puzzle answer was <code>([^<]+)</code>.</p>" :: String)
               in case (== answer) <$> answers !? (part - 1) of
                       Just True -> putStrLn "\n[CheckAnswer] That's the right answer!"
                       Just False -> do
                           putStrLn "\n[CheckAnswer] That's not the right answer."
                           exitFailure
                       Nothing -> do
                           putStrLn "\n[CheckAnswer] Puzzle not solved yet."
                           exitFailure
        400 -> do
            hPutStrLn stderr $ printf "[CheckAnswer] Server responded with 400 is your session cookie still valid? (%s)" $ trim body
            exitFailure
        _ -> do
            hPutStrLn stderr $ printf "[CheckAnswer] Code %d: %s" code body
            exitFailure
