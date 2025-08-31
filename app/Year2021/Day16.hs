module Year2021.Day16 (part1, part2) where

import Util
import Data.Char
import Data.List.Extra
import Data.Tuple.Extra

data Packet = Literal { version :: Int, value :: Int } |
              Operator { version :: Int, typeId :: Int, packets :: [Packet] }

hexToBin :: String -> String
hexToBin = concatMap (\case '0' -> "0000"; '1' -> "0001"; '2' -> "0010"; '3' -> "0011"; '4' -> "0100"
                            '5' -> "0101"; '6' -> "0110"; '7' -> "0111"; '8' -> "1000"; '9' -> "1001"
                            'A' -> "1010"; 'B' -> "1011"; 'C' -> "1100"; 'D' -> "1101"; 'E' -> "1110"
                            'F' -> "1111"; _   -> error "Invalid character in hex string.")

binToDec :: String -> Int
binToDec = foldl (\acc v -> acc * 2 + digitToInt v) 0

parseLiteral :: String -> String -> (Int, String)
parseLiteral ('0':b1:b2:b3:b4:xs) acc = (binToDec $ acc ++ [b1,b2,b3,b4], xs)
parseLiteral ('1':b1:b2:b3:b4:xs) acc = parseLiteral xs $ acc ++ [b1,b2,b3,b4]
parseLiteral _ _ = error "Invalid literal packet."

parseOperator :: String -> ([Packet], String)
parseOperator ('0':bits) = (unfoldr (\bs -> if null bs then Nothing else Just $ parsePacket bs) packets, rest2)
    where (len, rest1) = splitAt 15 bits
          (packets, rest2) = splitAt (binToDec len) rest1
parseOperator ('1':bits) = parseN (binToDec limit) rest
    where (limit, rest) = splitAt 11 bits
          parseN :: Int -> String -> ([Packet], String)
          parseN 0 rs = ([], rs)
          parseN n bs = let (packet, rs) = parsePacket bs
                        in first (packet:) $ parseN (n - 1) rs
parseOperator _ = error "Invalid operator packet."

parsePacket :: String -> (Packet, String)
parsePacket (v1:v2:v3:t1:t2:t3:bits@(_:_:_)) = case typeId of
                                                    4 -> first (\value -> Literal {..}) $ parseLiteral bits ""
                                                    _ -> first (\packets -> Operator {..}) $ parseOperator bits
    where version = binToDec [v1,v2,v3]
          typeId = binToDec [t1,t2,t3]
parsePacket _ = error "Invalid packet."

getVersionSum :: Packet -> Int
getVersionSum Literal {..} = version
getVersionSum Operator {..} = version + sumOn' getVersionSum packets

part1 :: Solution
part1 = V . getVersionSum . fst . parsePacket . hexToBin . hd


getValue :: Packet -> Int
getValue Literal {..} = value
getValue Operator {..} = (case typeId of
                               0 -> sum
                               1 -> product
                               2 -> minimum
                               3 -> maximum
                               5 -> fromEnum . uncurry (>) . tuple
                               6 -> fromEnum . uncurry (<) . tuple
                               7 -> fromEnum . uncurry (==) . tuple
                               _ -> error "Invalid type id.") $ map getValue packets

part2 :: Solution
part2 = V . getValue . fst . parsePacket . hexToBin . hd
