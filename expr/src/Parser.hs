module Parser where

import Text.Printf (printf)
import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt)
import Data.Function (on)

data Operator = Plus
              | Mult
              | Minus
              | Div
              | Pow
              deriving (Show, Eq)

toOp :: Char -> Operator
toOp '+' = Plus
toOp '*' = Mult
toOp '^' = Pow
toOp '-' = Minus
toOp '/' = Div
toOp c = error $ printf "Unsupported operator: %c" c

data Expr = BinOp Operator Expr Expr
          | Num Int
          deriving (Show, Eq)

eval :: Expr -> Int
eval (BinOp Plus l r) = eval l + eval r
eval (BinOp Minus l r) = eval l - eval r
eval (BinOp Mult l r) = eval l * eval r
eval (BinOp Div l r) = eval l `div` eval r
eval (BinOp Pow l r) = eval l ^ eval r
eval (Num x) = x

data ParserType = Prefix | Infix deriving (Show)

parse :: ParserType -> String -> Maybe Expr
parse pType str =
    case go pType str of
      Just ("", e) -> Just e
      _ -> Nothing
  where
    go Prefix = parsePrefix
    go Infix  = parseInfix

-- Expr :: + Expr Expr
--       | * Expr Expr
--       | - Expr Expr
--       | / Expr Expr
--       | ^ Expr Expr
--       | Digit
-- +1*234 -> Just ("4", ...)
parsePrefix :: String -> Maybe (String, Expr)
parsePrefix (op : t) | op `elem` "+-*/^" = do
  (t', l) <- parsePrefix t
  (t'', r) <- parsePrefix t'
  return $ (t'', BinOp (toOp op) l r)
parsePrefix (d : t) | isDigit d =
  Just (t, Num (digitToInt d))
parsePrefix _ = Nothing

-- Expr :: Expr + Expr
--       | Expr * Expr
--       | Expr - Expr
--       | Expr / Expr
--       | Expr ^ Expr
--       | Digit
--       | ( Expr )
-- Expr :: Слаг (+|-) Слаг (+|-) ... (+|-) Слаг
-- Слаг :: Множ ((*|/) Множ) (*|/) ... ((*|/) Множ) -> [Expr]
-- Множ :: Атом ^ Атом ^ ... ^ Атом
-- Атом :: Цифра | Выражение в скобках
parseInfix :: String -> Maybe (String, Expr)
parseInfix = parseSum

type BinOpFold = (
                   (Operator, Expr) -> (Operator, Expr) -> (Operator, Expr)
                 ) -> [(Operator, Expr)] -> (Operator, Expr)

type ParserFn a = String -> Maybe (String, a)

parseSum :: ParserFn Expr 
parseSum = parseBinOpL parseMult (\s -> parsePlus s <|> parseMinus s)
      
parseMult :: ParserFn Expr
parseMult = parseBinOpL parsePow (\s -> parseStar s <|> parseSlash s)

parsePow :: ParserFn Expr 
parsePow = parseBinOpR (\s -> parseDigit s <|> parseExprBr s) parseCap

parseBinOpL = parseBinOp foldl1
parseBinOpR = parseBinOp foldr1

parseBinOp :: BinOpFold -> ParserFn Expr -> ParserFn Operator -> ParserFn Expr
parseBinOp fold1 parseOperand parseOperator str =
    (binOp fold1 <$>) <$> go str undefined
  where
    go :: String -> Operator -> Maybe (String, [(Operator, Expr)])
    go str op = do
      (t, e) <- parseOperand str
      let maybeOp = parseOperator t
      rest <- (maybeOp >>= uncurry go) <|> Just (t, [])
      return $ ((op, e):) <$> rest


parseExprBr :: ParserFn Expr
parseExprBr ('(' : t) = do
   ((')' : t'), e) <- parseSum t
   return (t' ,e)
parseExprBr _ = Nothing

binOp :: BinOpFold -> [(Operator, Expr)] -> Expr
binOp fold1 = snd . (fold1 (\ (op1, e1) (op2, e2) -> (op1, BinOp op2 e1 e2)))

parsePlus :: ParserFn Operator
parsePlus ('+' : t) = Just (t, Plus)
parsePlus _ = Nothing

parseMinus :: ParserFn Operator
parseMinus ('-' : t) = Just (t, Minus)
parseMinus _ = Nothing

parseStar :: ParserFn Operator
parseStar ('*' : t) = Just (t, Mult)
parseStar _ = Nothing

parseSlash :: ParserFn Operator
parseSlash ('/' : t) = Just (t, Div)
parseSlash _ = Nothing

parseCap :: ParserFn Operator
parseCap ('^' : t) = Just (t, Pow)
parseCap _ = Nothing

parseDigit :: ParserFn Expr
parseDigit (d : t) | isDigit d =
  Just (t, Num (digitToInt d))
parseDigit _ = Nothing


plus :: Expr -> Expr -> Expr
plus = BinOp Plus

mult :: Expr -> Expr -> Expr
mult = BinOp Mult

pow :: Expr -> Expr -> Expr
pow = BinOp Pow

minus :: Expr -> Expr -> Expr
minus = BinOp Minus

divide :: Expr -> Expr -> Expr
divide = BinOp Div
