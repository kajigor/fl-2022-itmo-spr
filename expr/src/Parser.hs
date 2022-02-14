{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
module Parser where

import Text.Printf (printf)
import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt)
import Data.Maybe (fromJust, isNothing, fromMaybe)


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
parsePrefix (op : t) | op == '+' || op == '*' || op == '-' || op == '/' || op == '^' =
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
parseInfix  str = parseLeftAssociative str

parseLeftAssociative :: String -> Maybe (String, Expr)
parseLeftAssociative = parseUnique foldl1 parseRightAssociative parseOp

parseRightAssociative :: String -> Maybe (String, Expr)
parseRightAssociative = parseUnique foldr1 parseExpr parseOp

--чтобы неструктура парсера сохранилась, но уже возвращаем не просто выражение а пару (выражение, maybe оператор)
parseUnique op left right str = 
  let res = (op applyOp <$>) <$> go left right str 
        where 
          go left right str = 
                let first = left str in
                case first of
                Nothing -> Nothing
                Just (t, e) ->
                  if null t
                  then Just ("", [(e, Nothing)])
                  else
                    case right t of
                      Just (t', e') ->
                        let rest = go left right t' in
                        (((e, Just e'):) <$>) <$> rest
                      Nothing -> Just (t, [(e, Nothing)])
          applyOp left right | isNothing $ snd left = left
                             | otherwise = (BinOp (fromJust $ snd left) (fst left) (fst right), snd right)
    in case res of
      Nothing -> Nothing 
      Just (t, e) -> Just (t, fst e)
        
-- -- парсим операторы 
parseOp :: String -> Maybe (String, Operator)
parseOp (s : str) =
    if isDigit s then parseOp str
    else
      case s of 
        '+' -> Just (str, Plus)
        '-' -> Just (str, Minus)
        '/' -> Just (str, Div)
        '*' -> Just (str, Mult)
        '^' -> Just (str, Pow)
        _ -> Nothing 
parseOp [] = Nothing

--парсим цифры или скобки
parseExpr :: String -> Maybe (String, Expr)
parseExpr (x : xs) | isDigit x = Just (xs, Num (digitToInt x))
                   | null (x:xs) = Nothing
                   | x == '(' = a 
                    where
                      a =  case parseLeftAssociative xs of
                        Just (')' : t', e) -> Just (t', e)
                        _ -> Nothing

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