{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Infix where

import Expr
import Data.Char ( isDigit, isSpace, digitToInt )
import Combinators
import Control.Applicative ((<|>), some, many)
import Control.Monad (unless)

-- Expr :: Expr - Expr | Expr + Expr (Левоассоциативно)
--       | Expr * Expr | Expr / Expr (Левоассоциативно)
--       | Expr ^ Expr               (Правоассоциативно)
--       | Digit
--       | ( Expr )
-- Expr :: Слаг + Слаг + ... + Слаг
-- Слаг :: Множ (* Множ) * ... (* Множ) -> [Expr]
-- Множ :: Цифра | Выражение в скобках
parseInfix :: String -> Maybe (String, Expr)
parseInfix = runParser (spaced parseSum)

parseSum :: Parser Expr
parseSum = leftAssoc toBinOp <$> list parseMult (parsePlus <|> parseMinus)

parseMult :: Parser Expr
parseMult = leftAssoc toBinOp <$> list parsePow (parseStar <|> parseDiv)

parsePow :: Parser Expr
parsePow = rightAssoc toBinOp <$> list (spaced (Num <$> parseNaturalNumber) <|> parseExprBr) parseHat

toBinOp :: Expr -> Operator -> Expr -> Expr
toBinOp l op r = BinOp op l r

parseExprBr :: Parser Expr
parseExprBr = do
  spaced (char '(')
  e <- parseSum
  spaced (char ')')
  return e

parsePlus :: Parser Operator
parsePlus = Plus <$ spaced (char '+')

parseMinus :: Parser Operator
parseMinus = Minus <$ spaced (char '-')

parseStar :: Parser Operator
parseStar = Mult <$ spaced (char '*')

parseDiv :: Parser Operator
parseDiv = Div <$ spaced (char '/')

parseHat :: Parser Operator
parseHat = Pow <$ spaced (char '^')

parseDigit :: Parser Expr
parseDigit = Parser $ \str ->
  case str of
    (d : t) | isDigit d -> Just (t, Num (digitToInt d))
    _ -> Nothing


parseNaturalNumber :: Parser Int
parseNaturalNumber = do
  nums <- some (satisfy isDigit item)
  return $ read nums

item :: Parser Char
item = Parser $ \str ->
  case str of
    (h : t) -> Just (t, h)
    _ -> Nothing

satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy p parser = do
  res <- parser
  if p res
  then return res
  else fail "Predicate failed"

space :: Parser Char
space = satisfy isSpace item

spaces :: Parser ()
spaces = () <$ many space

spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces


