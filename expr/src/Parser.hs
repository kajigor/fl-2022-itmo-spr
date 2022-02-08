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

type Parser a = String -> Maybe (String, a)

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
parsePrefix :: Parser Expr
parsePrefix [] = Nothing
parsePrefix (op : t)
  | op == '+' || op == '*' || op == '-' || op == '*' || op == '^' = do
    (t' , l) <- parsePrefix t
    (t'', r) <- parsePrefix t'
    return (t'', BinOp (toOp op) l r)
  | isDigit op = Just (t, Num (digitToInt op))
  | otherwise = Nothing
-- Expr :: Expr - Expr
--       | Expr * Expr
--       | Digit
--       | ( Expr )
-- Expr :: Слаг + Слаг + ... + Слаг
-- Слаг :: Множ (* Множ) * ... (* Множ) -> [Expr]
-- Множ :: Цифра | Выражение в скобках
parseInfix :: Parser Expr
parseInfix = parseLvl1

data Associativity = LeftAcc | RightAcc deriving (Eq)

parseLvl
  :: [Parser Operator]
  -> [Parser Expr]
  -> Associativity
  -> String
  -> Maybe (String, Expr)
parseLvl fops hops acc str | acc == LeftAcc = go str >>= g
                           | otherwise      = go str >>= g'
 where
  go :: String -> Maybe (String, [(Maybe Operator, Expr)])
  go str = foldl (\a x -> a <|> x str) Nothing hops >>= f

  f ("", e) = Just ("", [(Nothing, e)])
  f (t, e) =
    let make (t', op) = (((Just op, e) :) <$>) <$> go t'
    in  (foldl (\a x -> a <|> x t) Nothing fops >>= make)
          <|> return (t, [(Nothing, e)])

  g' (t, xs) = return (t, snd (foldr1 h xs))
  g (t, xs) = return (t, snd (foldl1 h xs))
  h (Just op, e) (ops, e') = (ops, BinOp op e e')
  h _            _         = undefined


parseLvl3 :: Parser Expr
parseLvl3 = parseLvl [parsePow] [parseDigit, parseExprBr] RightAcc

parseLvl2 :: Parser Expr
parseLvl2 = parseLvl [parseStar, parseDiv] [parseLvl3] LeftAcc

parseLvl1 :: Parser Expr
parseLvl1 = parseLvl [parsePlus, parseMinus] [parseLvl2] LeftAcc

parseExprBr :: Parser Expr
parseExprBr ('(' : t) = case parseLvl1 t of
  Just (')' : t', e) -> Just (t', e)
  _                  -> Nothing
parseExprBr _ = Nothing

binOp :: Operator -> [Expr] -> Expr
binOp op = foldl1 (BinOp op)

parsePlus :: Parser Operator
parsePlus ('+' : t) = Just (t, Plus)
parsePlus _         = Nothing

parseMinus :: Parser Operator
parseMinus ('-' : t) = Just (t, Minus)
parseMinus _         = Nothing

parseStar :: Parser Operator
parseStar ('*' : t) = Just (t, Mult)
parseStar _         = Nothing

parseDiv :: Parser Operator
parseDiv ('/' : t) = Just (t, Div)
parseDiv _         = Nothing

parsePow :: Parser Operator
parsePow ('^' : t) = Just (t, Pow)
parsePow _         = Nothing

parseDigit :: Parser Expr
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
