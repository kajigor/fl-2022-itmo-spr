{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Infix where

import Expr ( Expr(BinOp), Operator(..) )
import Combinators
    ( Parser(runParser),
      leftAssoc,
      rightAssoc,
      list,
      char,
      (<|>),
      parseDigit,
      listR )

-- Expr :: Expr - Expr | Expr + Expr (Левоассоциативно)
--       | Expr * Expr | Expr / Expr (Левоассоциативно)
--       | Expr ^ Expr               (Правоассоциативно)
--       | Digit
--       | ( Expr )
-- Expr :: Слаг + Слаг + ... + Слаг
-- Слаг :: Множ (* Множ) * ... (* Множ) -> [Expr]
-- Множ :: Цифра | Выражение в скобках
parseInfix :: String -> Maybe (String, Expr)
parseInfix = runParser parseSumDiff


parseSumDiff :: Parser Expr
parseSumDiff = leftAssoc toBinOp <$> list parseMultDiv (parsePlus <|> parseMinus)

parseMultDiv :: Parser Expr
parseMultDiv = leftAssoc toBinOp <$> list parsePow (parseStar <|> parseDiv)

parsePow :: Parser Expr
parsePow = rightAssoc toBinOp <$> listR (parseDigit <|> parseExprBr) parseHat

toBinOp :: Expr -> Operator -> Expr -> Expr
toBinOp l op r = BinOp op l r

parseExprBr :: Parser Expr
parseExprBr = do
  br1 <- char '('
  exp <- parseSumDiff
  br2 <- char ')'
  return exp

parsePlus :: Parser Operator
parsePlus = do
  res <- char '+'
  return Plus

parseMinus :: Parser Operator
parseMinus = do
  res <- char '-'
  return Minus

parseStar :: Parser Operator
parseStar = do
  res <- char '*'
  return Mult

parseDiv :: Parser Operator
parseDiv = do
  res <- char '/'
  return Div

parseHat :: Parser Operator
parseHat = do
  res <- char '^'
  return Pow
