module Parser where

import Text.Printf (printf)
import Control.Applicative ((<|>))
import Data.Char (isDigit, digitToInt)

type ExprParser = String -> Maybe (String, Expr)
type OpParser = String -> Maybe (String, Operator)

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

parsePrefix :: String -> Maybe (String, Expr)
parsePrefix (op : t) | elem op ['+', '*', '/', '-', '^'] =
  case parsePrefix t of
    Just (t', l) ->
      case parsePrefix t' of
        Just (t'', r) -> Just (t'', BinOp (toOp op) l r)
        Nothing -> Nothing
    Nothing -> Nothing
parsePrefix (d : t) | isDigit d =
  Just (t, Num (digitToInt d))
parsePrefix _ = Nothing

parseInfix :: ExprParser
parseInfix = parseSumSub

composeParsers :: Bool -> ExprParser -> ExprParser -> OpParser -> OpParser -> ExprParser
composeParsers right exprParser1 exprParser2 opParser1 opParser2 str  
-- применение правой или левой ассоциативности, получилось очень страшно,
-- но зато какие красивые парсеры на выходе
  | right = ((\(exps,ops) -> foldr (\x a -> x a) (last exps) (zipWith (BinOp) ops (init exps))) <$>) <$> (go str)
  | otherwise = ((\(exps,ops) -> foldl (\a x -> x a) (head exps) (zipWith (\o l r -> BinOp o r l) ops (tail exps))) <$>) <$> (go str)
  where 
    go :: String -> Maybe (String, ([Expr], [Operator]))
    go str =
      let first = exprParser1 str <|> exprParser2 str in
      case first of
        Nothing -> Nothing
        Just (t, e) ->
          if null t
          then Just ("", ([e], []))
          else
            case opParser1 t <|> opParser2 t of
              Just (t', op) ->
                let rest = go t' in case rest of
                  Nothing -> Nothing
                  Just(t'', (exprs, ops)) -> Just(t'', (e:exprs, op:ops))
              Nothing -> Just (t, ([e],[]))


parseSumSub :: ExprParser
parseSumSub = composeParsers False parseMultDiv parseExprBr parsePlus parseMinus

parseMultDiv :: ExprParser
parseMultDiv = composeParsers False parsePow parseExprBr parseStar parseSlash

parsePow :: ExprParser
parsePow = composeParsers True parseDigit parseExprBr parseCaret (const Nothing)

parseExprBr :: ExprParser
parseExprBr ('(' : t) =
  case parseSumSub t of
    Just ((')' : t'), e) -> Just (t', e)
    _ -> Nothing
parseExprBr _ = Nothing

binOp :: Operator -> [Expr] -> Expr
binOp Pow = foldr1 (BinOp Pow)
binOp op = foldl1 (BinOp op)

parsePlus :: OpParser
parsePlus ('+' : t) = Just (t, Plus)
parsePlus _ = Nothing

parseMinus :: OpParser
parseMinus ('-' : t) = Just (t, Minus)
parseMinus _ = Nothing

parseStar :: OpParser
parseStar ('*' : t) = Just (t, Mult)
parseStar _ = Nothing

parseSlash :: OpParser
parseSlash ('/' : t) = Just (t, Div)
parseSlash _ = Nothing

parseCaret :: OpParser
parseCaret ('^' : t) = Just (t, Pow)
parseCaret _ = Nothing

parseDigit :: ExprParser
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