{-# LANGUAGE InstanceSigs #-}

module Combinators where

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }


leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  foldl (\acc (sep, elem) -> f acc sep elem) first rest


rightAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
rightAssoc f (first, rest) =
    let (beginning, last) = go (first, rest) in
    foldr (\(elem, sep) acc -> f elem sep acc) last beginning
  where
    go :: (elem, [(sep, elem)]) -> ([(elem, sep)], elem)
    go (first, []) = ([], first)
    go (first, ((sep, second) : rest)) =
      let (list, last) = go (second, rest) in
      ((first, sep) : list, last)


-- Expr :: Expr - Expr | Expr + Expr (Левоассоциативно)
--       | Expr * Expr | Expr / Expr (Левоассоциативно)
--       | Expr ^ Expr               (Правоассоциативно)
--       | Digit
--       | ( Expr )

-- Expr (op Expr) (op Expr) ... (op Expr) -> (Expr, [(op, Expr)])
list :: Parser expr -> Parser sep -> Parser (expr, [(sep, expr)])
list expr sep = do
  first <- expr
  rest <- goParser <|> return [] -- т.к. может не быть продолжения
  return (first, rest)
  where
    goParser = do
      sep' <- sep
      expr' <- expr
      res <- goParser <|> return [] -- т.к. продолжение имеет конец
      return ((sep', expr') : res)


instance Functor Parser where
  fmap f p = Parser $ \str ->
    case runParser p str of
      Just (s', res) -> Just (s', f res)
      Nothing -> Nothing


instance Applicative Parser where
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $ \str ->
    case runParser p1 str of
      Just (str', f) ->
        case runParser p2 str' of
          Just (str'', x) -> Just (str'', f x)
          Nothing -> Nothing
      Nothing -> Nothing

  pure :: a -> Parser a
  pure x = Parser $ \str -> Just (str, x)


instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= k = Parser $ \str ->
    case runParser p str of
      Just (str', res) -> runParser (k res) str'
      Nothing -> Nothing

  return :: a -> Parser a
  return = pure


char :: Char -> Parser Char
char c = Parser $ \str ->
  case str of
    (h : t) | c == h -> Just (t, c)
    _ -> Nothing


(<|>) :: Parser a -> Parser a -> Parser a
l <|> r = Parser $ \str ->
  case runParser l str of
    Just (str', res) -> Just (str', res)
    Nothing -> runParser r str
