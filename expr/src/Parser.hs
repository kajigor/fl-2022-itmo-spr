module Parser where

import           Text.Printf                    ( printf )
import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( isDigit
                                                , digitToInt
                                                )

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
parsePrefix (op : t)
    | op == '+' || op == '*' || op == '-' || op == '/' || op == '^'
    = case parsePrefix t of
        Just (t', l) -> case parsePrefix t' of
            Just (t'', r) -> Just (t'', BinOp (toOp op) l r)
            Nothing       -> Nothing
        Nothing -> Nothing
parsePrefix (d : t) | isDigit d = Just (t, Num (digitToInt d))
parsePrefix _                   = Nothing

-- Expr :: Expr - Expr
--       | Expr + Expr
--       | Expr / Expr
--       | Expr * Expr
--       | Expr ^ Expr
--       | Digit
--       | ( Expr )
-- Expr :: Слаг + Слаг + ... + Слаг
-- Слаг :: Множ (* Множ) * ... (* Множ) -> [Expr]
-- Множ :: Цифра | Выражение в скобках

parseInfix :: String -> Maybe (String, Expr)
parseInfix = parseSumSub

parseUniversal
    :: Operator
    -> (String -> Maybe (String, Operator))
    -> -- parser of operator
       (String -> Maybe (String, Expr))
    -> -- parser first expr
       Maybe Expr
    -> String
    -> Maybe (String, Expr)
parseUniversal op parseOp parserFirst mExpr str = case mExpr of
    Nothing -> (binOp op <$>) <$> go str
    Just e  -> (binOp op <$>) <$> (((e :) <$>) <$> (go str))
  where
    go :: String -> Maybe (String, [Expr])
    go str =
        let first = parserFirst str
        in  case first of
                Nothing     -> Nothing
                Just (t, e) -> if null t
                    then Just ("", [e])
                    else case parseOp t of
                        Just (t', _) ->
                            let rest = go t' in ((e :) <$>) <$> rest
                        Nothing -> Just (t, [e])

parseUU
    :: (Operator, String -> Maybe (String, Operator))
    -> [(Operator, String -> Maybe (String, Operator))]
    -> (String -> Maybe (String, Expr))
    -> Maybe Expr
    -> String
    -> Maybe (String, Expr)
parseUU (op, p) parsers parseLow mExpr str =
    let tmp = parseUniversal op p parseLow mExpr str
    in  case tmp of
            Nothing            -> Nothing
            res@(Just ("", t)) -> res
            Just (restStr, e)  -> helper parsers              where
                helper ((op', p') : ps) = case p' restStr of
                    Just (restStr', _) ->
                        parseUU (op', p') parsers parseLow (Just e) restStr'
                    Nothing -> helper ps
                helper [] = tmp


parseSumSub :: String -> Maybe (String, Expr)
parseSumSub = parseUU (Plus, parsePlus)
                      [(Plus, parsePlus), (Minus, parseMinus)]
                      parseDivMult
                      Nothing

parseDivMult :: String -> Maybe (String, Expr)
parseDivMult = parseUU (Div, parseZnDiv)
                       [(Div, parseZnDiv), (Mult, parseStar)]
                       parsePow
                       Nothing

parsePow :: String -> Maybe (String, Expr)
parsePow =
    parseUniversal Pow parseZnPow (\s -> parseDigit s <|> parseExprBr s) Nothing

parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) = case parseSumSub t of
    Just ((')' : t'), e) -> Just (t', e)
    _                    -> Nothing
parseExprBr _ = Nothing

binOp :: Operator -> [Expr] -> Expr
binOp op | op == Plus || op == Minus || op == Div || op == Mult =
    foldl1 (BinOp op)
binOp Pow = foldr1 (BinOp Pow)

parseSign :: Char -> Operator -> String -> Maybe (String, Operator)
parseSign s op (c : t) | (c == s) = Just (t, op)
parseSign _ _ _                   = Nothing

parsePlus :: String -> Maybe (String, Operator)
parsePlus = parseSign '+' Plus

parseStar :: String -> Maybe (String, Operator)
parseStar = parseSign '*' Mult

parseMinus :: String -> Maybe (String, Operator)
parseMinus = parseSign '-' Minus

parseZnDiv :: String -> Maybe (String, Operator)
parseZnDiv = parseSign '/' Div

parseZnPow :: String -> Maybe (String, Operator)
parseZnPow = parseSign '^' Pow

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
