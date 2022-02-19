module Prefix where

import Expr ( Expr(BinOp), Operator, toOp )
import Combinators ( Parser(..), (<|>), parseDigit )
import Data.Char ( isDigit, digitToInt )
import Infix
    ( parsePlus, parseMinus, parseStar, parseDiv, parseHat ) 

parsePrefix :: String -> Maybe (String, Expr)
parsePrefix = runParser innerPrefix

innerPrefix :: Parser Expr
innerPrefix = parseDigit <|> parseExp

parseExp :: Parser Expr
parseExp = do
  op <- parseOp
  exp1 <- innerPrefix
  exp2 <- innerPrefix
  return $ BinOp op exp1 exp2

parseOp :: Parser Operator 
parseOp =  parsePlus <|> parseMinus <|> parseStar <|> parseDiv <|> parseHat