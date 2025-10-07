{-# LANGUAGE OverloadedStrings #-}

module Util.Http where

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
        200 -> let answers = map lst $ body =~ ("<p>Your puzzle answer was <code>([^<]+)</code>.</p>" :: String)
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
