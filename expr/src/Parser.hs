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
parseSum str =
  let f = foldl1
        (\(e1, mop1) (e2, mop2) -> case mop1 of
          Just op1 -> (BinOp op1 e1 e2, mop2)
          Nothing  -> (e1, mop1)
        )
  in  (fmap . fmap) (fst . f) (go str)
 where
  go :: String -> Maybe (String, [(Expr, Maybe Operator)])
  go str =
    let first = parseMult str
    in  case first of
          Just ("", e) -> Just ("", [(e, Nothing)])
          Just (t , e) -> case parsePlus t <|> parseMinus t of
            Just (t', op) ->
              let rest = go t' in fmap (((e, Just op) :) <$>) rest
            Nothing -> Just (t, [(e, Nothing)])
          _ -> Nothing

parseMult :: String -> Maybe (String, Expr)
parseMult str =
  let f = foldl1
        (\(e1, mop1) (e2, mop2) -> case mop1 of
          Just op1 -> (BinOp op1 e1 e2, mop2)
          Nothing  -> (e1, mop1)
        )
  in  (fmap . fmap) (fst . f) (go str)
 where
  go :: String -> Maybe (String, [(Expr, Maybe Operator)])
  go str =
    let first = parsePower str
    in  case first of
          Just ("", e) -> Just ("", [(e, Nothing)])
          Just (t , e) -> case parseStar t <|> parseDiv t of
            Just (t', op) ->
              let rest = go t' in fmap (((e, Just op) :) <$>) rest
            Nothing -> Just (t, [(e, Nothing)])
          _ -> Nothing

parsePower :: String -> Maybe (String, Expr)
parsePower str =
  let first = parseDigit str <|> parseExprBr str
  in  case first of
        Just (""  , e) -> Just ("", e)
        Just (rest, e) -> case parsePow rest of
          Just (rest', _) -> (fmap . fmap) (pow e) (parsePower rest')
          Nothing         -> Just (rest, e)
        _ -> Nothing

parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) = case parseSum t of
  Just (')' : t', e) -> Just (t', e)
  _                  -> Nothing
parseExprBr _ = Nothing

binOp :: Operator -> [Expr] -> Expr
binOp op = foldl1 (BinOp op)

parsePlus :: String -> Maybe (String, Operator)
parsePlus ('+' : t) = Just (t, Plus)
parsePlus _         = Nothing

parseStar :: String -> Maybe (String, Operator)
parseStar ('*' : t) = Just (t, Mult)
parseStar _         = Nothing

parseMinus :: String -> Maybe (String, Operator)
parseMinus ('-' : rest) = Just (rest, Minus)
parseMinus _            = Nothing

parseDiv :: String -> Maybe (String, Operator)
parseDiv ('/' : rest) = Just (rest, Div)
parseDiv _            = Nothing

parsePow :: String -> Maybe (String, Operator)
parsePow ('^' : rest) = Just (rest, Pow)
parsePow _            = Nothing

parseDigit :: String -> Maybe (String, Expr)
parseDigit (d : t) | isDigit d = Just (t, Num (digitToInt d))
parseDigit _                   = Nothing


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
