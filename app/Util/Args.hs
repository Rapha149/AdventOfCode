module Util.Args where

import Data.Char
import Data.List.Extra

data Source = File | Stdin | FileStdin | Clipboard | WaylandClipboard
data Flag = MeasureTime | AutoAnswer | CheckAnswer deriving Eq
data Args = ArgError String | Options { flags :: [Flag], year :: Int, day :: Int, part :: Int, source :: Source, extra :: [String]}

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
parseArgs (_:_:_:source:_) | lower source `notElem` ["file", "stdin", "fstdin", "clip", "wclip"] = ArgError "Specify 'file', 'stdin', 'fstdin', 'clip' or 'wclip' for the source."
parseArgs (year:day:part:source:extra) = Options { flags = [],
                                                   year = read year,
                                                   day = read day,
                                                   part = read part,
                                                   source = case lower source of
                                                                 "file" -> File
                                                                 "stdin" -> Stdin
                                                                 "fstdin" -> FileStdin
                                                                 "clip" -> Clipboard
                                                                 "wclip" -> WaylandClipboard
                                                                 _ -> error "Unknown source argument.",
                                                   extra }
parseArgs _ = ArgError "Illegal number of arguments."
