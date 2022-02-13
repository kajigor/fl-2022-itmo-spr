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
--       | - Expr Expr
--       | / Expr Expr
--       | ^ Expr Expr
--       | Digit
-- +1*234 -> Just ("4", ...)
parsePrefix :: String -> Maybe (String, Expr)
parsePrefix (op : t) | op `elem` "+-*/^" = do
  (t', l) <- parsePrefix t
  (t'', r) <- parsePrefix t'
  return $ (t'', BinOp (toOp op) l r)
parsePrefix (d : t) | isDigit d =
  Just (t, Num (digitToInt d))
parsePrefix _ = Nothing

-- Expr :: Expr + Expr
--       | Expr * Expr
--       | Expr - Expr
--       | Expr / Expr
--       | Expr ^ Expr
--       | Digit
--       | ( Expr )
-- Expr :: Слаг (+|-) Слаг (+|-) ... (+|-) Слаг
-- Слаг :: Множ ((*|/) Множ) (*|/) ... ((*|/) Множ) -> [Expr]
-- Множ :: Атом ^ Атом ^ ... ^ Атом
-- Атом :: Цифра | Выражение в скобках
parseInfix :: String -> Maybe (String, Expr)
parseInfix = parseSum

parseSum :: String -> Maybe (String, Expr)
parseSum str =
    (binOp foldl1 <$>) <$> go str undefined
  where
    go :: String -> Operator -> Maybe (String, [(Operator, Expr)])
    go str op = do
      (t, e) <- parseMult str
      let maybeOp = parsePlus t <|> parseMinus t
      rest <- (maybeOp >>= uncurry go) <|> Just (t, [])
      return $ ((op, e):) <$> rest
      
parseMult :: String -> Maybe (String, Expr)
parseMult str =
    (binOp foldl1 <$>) <$> go str undefined
  where
    go :: String -> Operator -> Maybe (String, [(Operator, Expr)])
    go str op = do
      (t, e) <- parsePow str
      let maybeOp = parseStar t <|> parseSlash t
      rest <- (maybeOp >>= uncurry go) <|> Just (t, [])
      return $ ((op, e):) <$> rest

parsePow :: String -> Maybe (String, Expr)
parsePow str = 
    (binOp foldr1 <$>) <$> go str undefined
  where
    go :: String -> Operator -> Maybe (String, [(Operator, Expr)])
    go str op = do
      (t, e) <- parseDigit str <|> parseExprBr str
      let maybeOp = parseCap t
      rest <- (maybeOp >>= uncurry go) <|> Just (t, [])
      return $ ((op, e):) <$> rest
    
parseExprBr :: String -> Maybe (String, Expr)
parseExprBr ('(' : t) = do
   ((')' : t'), e) <- parseSum t
   return (t' ,e)
parseExprBr _ = Nothing

binOp :: (
           (
             (Operator, Expr) -> (Operator, Expr) -> (Operator, Expr)
           ) -> [(Operator, Expr)] -> (Operator, Expr)
         ) -> [(Operator, Expr)] -> Expr
binOp fold1 = snd . (fold1 (\ (op1, e1) (op2, e2) -> (op1, BinOp op2 e1 e2)))

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

parseCap :: String -> Maybe (String, Operator)
parseCap ('^' : t) = Just (t, Pow)
parseCap _ = Nothing

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
