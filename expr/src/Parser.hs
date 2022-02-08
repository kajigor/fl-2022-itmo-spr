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

-- Префиксная форма
parsePrefix :: String -> Maybe (String, Expr)
parsePrefix (d : t)
    | d == '+' || d == '*' || d == '-' || d == '/' || d == '^' =
        case parsePrefix t of
            Just (t', l) ->
                case parsePrefix t' of
                    Just (t'', r) -> Just (t'', BinOp (toOp d) l r)
                    Nothing -> Nothing
            Nothing -> Nothing
    | isDigit d = Just (t, Num (digitToInt d))
    | otherwise = Nothing
parsePrefix _ = Nothing


-- Инфиксная форма
parseInfix :: String -> Maybe (String, Expr)
parseInfix str = parseExpr str 0


-- Выражение на верхнем уровне: + или -
-- Ниже уровнем: * или /
-- Еще ниже: ^
-- В самом низу: число или скобка
parseExpr :: String -> Int -> Maybe (String, Expr)
parseExpr str d =
    case res of
        Just (t, (x:xs, ys)) -> if d == 2 -- для ^
                                then Just $ goRight (t, (x:xs, ys))
                                else Just $ goLeft x (t, (xs, ys))
        Nothing -> Nothing
    where
        res = go str

        -- Т.к. теперь + и -, а также * и / имеют один приоритет, то хотим уметь
        -- сохранять список с ними (нельзя как раньше просто сделать свертку с Plus)
        -- Отсюда и берется [Operator]. Он для d=0 хранит [Plus, Minus,...], для
        -- d=1 хранит [Mult, Div,...], а для d=2 хранит [Pow,...]
        go :: String -> Maybe (String, ([Expr], [Operator]))
        go str =
            let first = (if (d < 2)
                        then parseExpr str (d + 1)
                        else parseDigit str) <|> parseExprBr str
                        in
            case first of
              Nothing -> Nothing
              Just (t, e) ->
                if null t
                then Just ("", ([e], []))
                else
                    case parseOperator t d of
                    Just (t', op) ->
                      let rest = go t' in
                      appendToPair (e, op) rest
                    Nothing -> Just (t, ([e], []))

        goLeft :: Expr -> (String, ([Expr], [Operator])) -> (String, Expr)
        goLeft ini (t, (x:xs, y:ys)) = goLeft (BinOp y ini x) (t, (xs, ys))
        goLeft ini (t, ([], [])) = (t, ini)

        goRight :: (String, ([Expr], [Operator])) -> (String, Expr)
        goRight (t, (e, _)) = (t, foldr1 (BinOp Pow) e)

        appendToPair :: (Expr, Operator) -> Maybe (String, ([Expr], [Operator])) -> Maybe (String, ([Expr], [Operator]))
        appendToPair (e, op) (Just (t, (l1, l2))) = Just (t, (e:l1, op:l2))
        appendToPair _ Nothing = Nothing



parseOperator :: String -> Int -> Maybe (String, Operator)
parseOperator ('+':t) 0 = Just (t, Plus)
parseOperator ('-':t) 0 = Just (t, Minus)
parseOperator ('*':t) 1 = Just (t, Mult)
parseOperator ('/':t) 1 = Just (t, Div)
parseOperator ('^':t) 2 = Just (t, Pow)
parseOperator _ _ = Nothing


parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) =
  case parseExpr t 0 of
    Just ((')' : t'), e) -> Just (t', e)
    _ -> Nothing
parseExprBr _ = Nothing


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
