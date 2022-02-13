module Parser where

import Text.Printf (printf)
import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt)

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
--       | Digit
-- +1*234 -> Just ("4", ...)
parsePrefix :: String -> Maybe (String, Expr)
parsePrefix (op : t) | op == '+' || op == '*' || op == '^' || op == '-' || op == '/' =
  case parsePrefix t of
    Just (t', l) ->
      case parsePrefix t' of
        Just (t'', r) -> Just (t'', BinOp (toOp op) l r)
        Nothing -> Nothing
    Nothing -> Nothing
parsePrefix (d : t) | isDigit d =
  Just (t, Num (digitToInt d))
parsePrefix _ = Nothing

-- Expr :: Expr - Expr
--       | Expr * Expr
--       | Digit
--       | ( Expr )
-- Expr :: Слаг + Слаг + ... + Слаг
-- Слаг :: Множ (* Множ) * ... (* Множ) -> [Expr]
-- Множ :: Цифра | Выражение в скобках
parseInfix :: String -> Maybe (String, Expr)
parseInfix = parseSum

parseExpr :: Bool
     -> Operator
     -> (String -> Maybe (String, Expr))
     -> (String -> Maybe (String, Operator))
     -> String
     -> Maybe (String, Expr)
parseExpr isLeft op fst snd str =
    (binOp op <$>) <$> go str
  where
    binOp :: Operator -> [Expr] -> Expr
    binOp | isLeft    = binOpL
          | otherwise = binOpR
    go :: String -> Maybe (String, [Expr])
    go str =
      let first = fst str in
      case first of
        Nothing -> Nothing
        Just (t, e) ->
          if null t
          then Just ("", [e])
          else
            case snd t of
              Just (t', _) ->
                let rest = go t' in
                ((e:) <$>) <$> rest
              Nothing -> Just (t, [e])

parseSum :: String -> Maybe (String, Expr)
parseSum str = parseExpr True Plus parseMult (\s -> parsePlus s <|> parseDash s) str

parseMult :: String -> Maybe (String, Expr)
parseMult str = parseExpr True Mult parsePower (\s -> parseStar s <|> parseSlash s) str

parsePower :: String -> Maybe (String, Expr)
parsePower str = parseExpr False Pow (\s -> parseDigit s <|> parseExprBr s) parseHat str

parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) =
  case parseSum t of
    Just ((')' : t'), e) -> Just (t', e)
    _ -> Nothing
parseExprBr _ = Nothing

binOpL :: Operator -> [Expr] -> Expr
binOpL op = foldl1 (BinOp op)

binOpR :: Operator -> [Expr] -> Expr
binOpR op = foldr1 (BinOp op)

parseOpSymbol :: Char -> Operator -> String -> Maybe (String, Operator)
parseOpSymbol symbol op (c : t) | c == symbol = Just (t, op)
                                | otherwise   = Nothing

parsePlus :: String -> Maybe (String, Operator)
parsePlus = parseOpSymbol '+' Plus

parseStar :: String -> Maybe (String, Operator)
parseStar = parseOpSymbol '*' Mult

parseDash :: String -> Maybe (String, Operator)
parseDash = parseOpSymbol '-' Minus

parseSlash :: String -> Maybe (String, Operator)
parseSlash = parseOpSymbol '/' Div

parseHat :: String -> Maybe (String, Operator)
parseHat = parseOpSymbol '^' Pow

parseDigit :: String -> Maybe (String, Expr)
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