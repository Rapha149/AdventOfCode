module Years.Year2020.Day18 (part1, part2) where

import Util.Util
import Data.Char
import Data.List.Extra
import Data.Tuple.Extra

data Element = Add | Mult | Number Int | Parentheses Expression deriving (Show, Eq)
type Expression = [Element]

parseExpression :: String -> (Expression, String)
parseExpression "" = ([], "")
parseExpression ('+':' ':xs) = first (Add :) $ parseExpression xs
parseExpression ('*':' ':xs) = first (Mult :) $ parseExpression xs
parseExpression ('(':xs) = first (Parentheses expr :) $ parseExpression rest
    where (expr, rest) = parseExpression xs
parseExpression (')':xs) = ([], trim xs)
parseExpression xs = first (Number (read num) :) $ parseExpression $ trim rest
    where (num, rest) = span isDigit xs

evaluate1 :: Expression -> Int
evaluate1 [Number n] = n
evaluate1 (a:op:b:xs) = evaluate1 (Number res:xs)
    where (valA, valB) = both (\case Number n -> n
                                     Parentheses expr -> evaluate1 expr
                                     _ -> error "Invalid expression") (a, b)
          res = case op of
                     Add -> valA + valB
                     Mult -> valA * valB
                     _ -> error "Invalid expression."
evaluate1 _ = error "Invalid expression."

part1 :: Solution
part1 = V . sumOn' (evaluate1 . fst . parseExpression)


evaluate2 :: Expression -> Int
evaluate2 = productOn' (sumOn' (\case [Number n] -> n
                                      [Parentheses expr] -> evaluate2 expr
                                      _ -> error "Invalid expression") . split (== Add)) . split (== Mult)

part2 :: Solution
part2 = V . sumOn' (evaluate2 . fst . parseExpression)
