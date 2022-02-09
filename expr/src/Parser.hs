module Parser where

import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( digitToInt
                                                , isDigit
                                                )
import           Text.Printf                    ( printf )

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
toOp c   = error $ printf "Unsupported operator: %c" c

data Expr = BinOp Operator Expr Expr
          | Num Int
          deriving (Show, Eq)

eval :: Expr -> Int
eval (BinOp Plus  l r) = eval l + eval r
eval (BinOp Minus l r) = eval l - eval r
eval (BinOp Mult  l r) = eval l * eval r
eval (BinOp Div   l r) = eval l `div` eval r
eval (BinOp Pow   l r) = eval l ^ eval r
eval (Num x          ) = x

data ParserType = Prefix | Infix deriving (Show)

parse :: ParserType -> String -> Maybe Expr
parse pType str = case go pType str of
  Just ("", e) -> Just e
  _            -> Nothing
 where
  go Prefix = parsePrefix
  go Infix  = parseInfix

-- Expr :: + Expr Expr
--       | * Expr Expr
--       | Digit
-- +1*234 -> Just ("4", ...)
parsePrefix :: String -> Maybe (String, Expr)
parsePrefix (op : t) | isOperation op = case parsePrefix t of
  Just (t', l) -> case parsePrefix t' of
    Just (t'', r) -> Just (t'', BinOp (toOp op) l r)
    Nothing       -> Nothing
  Nothing -> Nothing
parsePrefix (d : t) | isDigit d = Just (t, Num (digitToInt d))
parsePrefix _                   = Nothing

-- Expr :: Expr - Expr
--       | Expr * Expr
--       | Digit
--       | ( Expr )
-- Expr :: Слаг + Слаг + ... + Слаг
-- Слаг :: Множ (* Множ) * ... (* Множ) -> [Expr]
-- Множ :: Цифра | Выражение в скобках
parseInfix :: String -> Maybe (String, Expr)
parseInfix = parseSum

parseSum :: String -> Maybe (String, Expr)
parseSum = genericParser parseMult (\s -> parsePlus s <|> parseMinus s)

parseMult :: String -> Maybe (String, Expr)
parseMult = genericParser parsePower (\s -> parseStar s <|> parseDiv s)

parsePower :: String -> Maybe (String, Expr)
parsePower str =
  let first = parseDigit str <|> parseExprBr str
  in  case first of
        Just (""  , e) -> Just ("", e)
        Just (rest, e) -> case parsePow rest of
          Just (rest', _) -> (fmap . fmap) (pow e) (parsePower rest')
          Nothing         -> Just (rest, e)
        _ -> Nothing


genericParser
  :: (String -> Maybe (String, Expr))
  -> (String -> Maybe (String, Operator))
  -> String
  -> Maybe (String, Expr)
genericParser f g s =
  let f = foldl1
        (\(e1, mop1) (e2, mop2) -> case mop1 of
          Just op1 -> (BinOp op1 e1 e2, mop2)
          Nothing  -> (e1, mop1)
        )
  in  (fmap . fmap) (fst . f) (go s)
 where
  go :: String -> Maybe (String, [(Expr, Maybe Operator)])
  go str =
    let first = f str
    in  case first of
          Just (""  , e) -> Just ("", [(e, Nothing)])
          Just (rest, e) -> case g rest of
            Just (rest', op) -> (fmap . fmap) ((e, Just op) :) (go rest')
            Nothing          -> Just (rest, [(e, Nothing)])
          _ -> Nothing

parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) = case parseSum t of
  Just (')' : t', e) -> Just (t', e)
  _                  -> Nothing
parseExprBr _ = Nothing

binOp :: Operator -> [Expr] -> Expr
binOp op = foldl1 (BinOp op)

parsePlus :: String -> Maybe (String, Operator)
parsePlus = parseSymbol (const Plus) (== '+')

parseStar :: String -> Maybe (String, Operator)
parseStar = parseSymbol (const Mult) (== '*')

parseMinus :: String -> Maybe (String, Operator)
parseMinus = parseSymbol (const Minus) (== '-')

parseDiv :: String -> Maybe (String, Operator)
parseDiv = parseSymbol (const Div) (== '/')

parsePow :: String -> Maybe (String, Operator)
parsePow = parseSymbol (const Pow) (== '^')

parseDigit :: String -> Maybe (String, Expr)
parseDigit = parseSymbol (Num . digitToInt) isDigit

parseSymbol :: (Char -> a) -> (Char -> Bool) -> String -> Maybe (String, a)
parseSymbol f p (c : s) | p c = Just (s, f c)
parseSymbol _ _ _             = Nothing

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

isOperation :: Char -> Bool
isOperation c = c == '+' || c == '/' || c == '-' || c == '*' || c == '^'
