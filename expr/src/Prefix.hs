module Prefix where

import           Combinators                    ( Parser(runParser)
                                                , parseDigit
                                                , parseOperator
                                                )
import           Expr                           ( Expr(BinOp) )
import           GHC.Base                       ( (<|>) )

-- Expr :: + Expr Expr
--       | * Expr Expr
--       | Digit
-- +1*234 -> Just ("4", ...)
parsePrefix :: String -> Maybe (String, Expr)
parsePrefix = runParser atom

atom :: Parser Expr
atom = expr <|> digit

expr :: Parser Expr
expr = do
  op <- parseOperator
  l  <- atom
  BinOp op l <$> atom

digit :: Parser Expr
digit = parseDigit
