{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
module Parser where

import Text.Printf (printf)
import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt, GeneralCategory (MathSymbol))
import Data.Maybe (fromJust, isNothing, fromMaybe, isJust)


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
parseInfix str = parseLA str

parseAccLeft op symb1 symb2 = parseUnique foldl1 op (\str -> (<|>) (symb1 str) (symb2 str))

parseAccRight op symb1 symb2 = parseUnique foldr1 (\str -> (<|>) (symb1 str) (symb2 str)) op

parseLA :: String -> Maybe (String, Expr)
parseLA = parseAccLeft parseLAHighOrder parseMinus parsePlus

parseLAHighOrder :: String -> Maybe (String, Expr)
parseLAHighOrder = parseAccLeft parseRA parseSlash parseStar

parseRA :: String -> Maybe (String, Expr)
parseRA = parseAccRight parsePow parseDigit parseExprBr

--структура парсера сохранилась, но уже возвращаем не просто [выражение] а пару [(выражение, maybe оператор)]
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

parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) =
  case parseLA t of
    Just (')' : t', e) -> Just (t', e)
    _ -> Nothing
parseExprBr _ = Nothing

binOp :: Operator -> [Expr] -> Expr
binOp op = foldl1 (BinOp op)

parsePlus :: String -> Maybe (String, Operator)
parsePlus ('+' : t) = Just (t, Plus)
parsePlus _ = Nothing

parseMinus :: String -> Maybe (String, Operator)
parseMinus ('-' : t) = Just (t, Minus)
parseMinus _ = Nothing

parseStar :: String -> Maybe (String, Operator)
parseStar ('*' : t) = Just (t, Mult)
parseStar _ = Nothing

parseSlash :: String -> Maybe (String, Operator)
parseSlash ('/' : t) = Just (t, Div)
parseSlash _ = Nothing

parsePow :: String -> Maybe (String, Operator)
parsePow ('^' : t) = Just (t, Pow)
parsePow _ = Nothing

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