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

parsePrefix :: String -> Maybe (String, Expr)
parsePrefix (op : t) | op == '+' || op == '*' || op =='/' || op == '-' || op == '^' =
  case parsePrefix t of
    Just (t', l) ->
      case parsePrefix t' of
        Just (t'', r) -> Just (t'', BinOp (toOp op) l r)
        Nothing -> Nothing
    Nothing -> Nothing
parsePrefix (d : t) | isDigit d =
  Just (t, Num (digitToInt d))
parsePrefix _ = Nothing

parseInfix :: String -> Maybe (String, Expr)
parseInfix = parseSumDiff

parseSumDiff :: String -> Maybe (String, Expr)
parseSumDiff = parseOperators foldl1 parseMultDiv (\s -> parsePlus s <|> parseMinus s)

parseMultDiv :: String -> Maybe (String, Expr)
parseMultDiv = parseOperators foldl1 parseExp (\s -> parseStar s <|> parseDiv s)

parseExp :: String -> Maybe (String, Expr)
parseExp = parseOperators foldr1 (\s -> parseDigit s <|> parseExprBr s) parsePow

parseOperators :: (((Expr, Maybe Operator) -> (Expr, Maybe Operator) -> (Expr, Maybe Operator))
  -> [(Expr, Maybe Operator)] -> (Expr, Maybe Operator))
  -> (String -> Maybe (String, Expr))
  -> (String -> Maybe (String, Operator))
  -> String
  -> Maybe (String, Expr)
parseOperators f op1 op2 str =
  let fl = (f helper <$>) <$> go op1 op2 str in
        case fl of
          Nothing -> Nothing
          Just (t, (e,o)) -> Just (t, e)
  where
    go :: (String -> Maybe (String, Expr)) -> (String -> Maybe (String, Operator)) -> String -> Maybe (String, [(Expr, Maybe Operator)])
    go op1 op2 str =
      let first = op1 str in
      case first of
        Nothing -> Nothing
        Just (t, e) ->
          if null t
          then Just ("", [(e, Nothing)])
          else
            case op2 t of
              Just (t', o) ->
                let rest = go op1 op2 t' in
                (((e, Just o):) <$>) <$> rest
              Nothing -> Just (t, [(e, Nothing)])
    helper (e, Just op) (e', Nothing) = (BinOp op e e', Nothing)
    helper (e, Just op) (e', Just op') = (BinOp op e e', Just op')
    helper (e, Nothing) _ = (e, Nothing)

parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) =
  case parseSumDiff t of
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

parseDiv :: String -> Maybe (String, Operator)
parseDiv ('/' : t) = Just (t, Div)
parseDiv _ = Nothing

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