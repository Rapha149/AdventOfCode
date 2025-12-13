module Years.Year2020.Day04 (part1, part2) where

import Util.Util
import Text.Regex.TDFA
import Data.Char
import Data.List.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Passport = Map String String

parseInput :: [String] -> [Passport]
parseInput = map parsePassport . split null
    where parsePassport :: [String] -> Passport
          parsePassport ls = Map.fromList [(k, v) | [k, v] <- map (splitOn ":") $ words $ unwords ls]

hasFields :: Passport -> Bool
hasFields passport = all (`Map.member` passport) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part1 :: Solution
part1 = V . length . filter hasFields . parseInput


isValid :: (String, String) -> Bool
isValid (field, value) = case field of
        "byr" -> isNum && num >= 1920 && num <= 2002
        "iyr" -> isNum && num >= 2010 && num <= 2020
        "eyr" -> isNum && num >= 2020 && num <= 2030
        "hgt" -> let (height, unit) = span isDigit value
                     h :: Int = read height
                 in not (null height) && case unit of
                                              "cm" -> h >= 150 && h <= 193
                                              "in" -> h >= 59 && h <= 76
                                              _ -> False
        "hcl" -> value =~ "^#[0-9a-f]{6}$"
        "ecl" -> value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        "pid" -> isNum && length value == 9
        _ -> True
    where isNum = all isDigit value
          num :: Int = read value

part2 :: Solution
part2 = V . length . filter (\passport -> hasFields passport && all isValid (Map.toList passport)) . parseInput
