module Prefix where

import Expr
import Data.Char ( isDigit, digitToInt )
import Infix (parseDigit)
import Combinators
import GHC.Base (Alternative((<|>)))

parsePrefix :: String -> Maybe (String, Expr)
parsePrefix = runParser goParse
  where
    goParse :: Parser Expr
    goParse = parseDigit <|> do
        op <- char '+' <|> char '-' <|> char '*' <|> char '/' <|> char '^'
        l  <- goParse
        BinOp (toOp op) l <$> goParse
