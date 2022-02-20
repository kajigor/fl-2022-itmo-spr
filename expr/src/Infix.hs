{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Infix where

import Expr
import Data.Char ( isDigit, digitToInt )
import Combinators
import Control.Applicative

-- Expr :: Expr - Expr | Expr + Expr (Левоассоциативно)
--       | Expr * Expr | Expr / Expr (Левоассоциативно)
--       | Expr ^ Expr               (Правоассоциативно)
--       | Digit
--       | ( Expr )
-- Expr :: Слаг + Слаг + ... + Слаг
-- Слаг :: Множ (* Множ) * ... (* Множ) -> [Expr]
-- Множ :: Цифра | Выражение в скобках
parseInfix :: String -> Maybe (String, Expr)
parseInfix = runParser parseSum

parseSum :: Parser Expr
parseSum = leftAssoc toBinOp <$> list parseMult (parsePlus <|> parseMinus)

parseMult :: Parser Expr
parseMult = leftAssoc toBinOp <$> list parsePow (parseStar <|> parseDiv)

parsePow :: Parser Expr
parsePow = rightAssoc toBinOp <$> listR (parseDigit <|> parseExprBr) parseHat

toBinOp :: Expr -> Operator -> Expr -> Expr
toBinOp l op = BinOp op l

parseExprBr :: Parser Expr
parseExprBr = Parser $ \str ->
  case str of
    ('(' : t) ->
      case runParser parseSum t of
        Just (')' : t', e) -> Just (t', e)
        _ -> Nothing
    _ -> Nothing

parsePlus :: Parser Operator
parsePlus = Parser $ \str ->
  case str of
    ('+' : t) -> Just (t, Plus)
    _ -> Nothing

parseMinus :: Parser Operator
parseMinus = Parser $ \str ->
  case str of
    ('-' : t) -> Just (t, Minus)
    _ -> Nothing

parseStar :: Parser Operator
parseStar = Parser $ \str ->
  case str of
    ('*' : t) -> Just (t, Mult)
    _ -> Nothing

parseDiv :: Parser Operator
parseDiv = Parser $ \str ->
  case str of
    ('/' : t) -> Just (t, Div)
    _ -> Nothing

parseHat :: Parser Operator
parseHat = Parser $ \str ->
  case str of
    ('^' : t) -> Just (t, Pow)
    _ -> Nothing

parseDigit :: Parser Expr
parseDigit = Parser $ \str ->
  case str of
    (d : t) | isDigit d -> Just (t, Num (digitToInt d))
    _ -> Nothing
