module Combinators where

import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

-- first [(sep, second), (sep', third)] -> sep' (sep (first, second)) third

leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  foldl (\acc (sep, elem) -> f acc sep elem) first rest

rightAssoc :: (elem -> sep -> elem -> elem) -> ([(elem, sep)], elem) -> elem
rightAssoc f (rest, last) =
  foldr (\(elem, sep) acc -> f elem sep acc) last rest

listR :: Parser elem -> Parser sep -> Parser ([(elem, sep)], elem)
listR elem sep = do
  res <- goParser <|> return []
  e <- elem
  return (res, e)
  where
    goParser = do
      elem' <- elem
      sep' <- sep
      res <- goParser <|> return []
      return $ (elem', sep') : res

-- Expr :: Expr - Expr | Expr + Expr (Левоассоциативно)
--       | Expr * Expr | Expr / Expr (Левоассоциативно)
--       | Expr ^ Expr               (Правоассоциативно)
--       | Digit
--       | ( Expr )

-- Expr (op Expr) (op Expr) ... (op Expr) -> (Expr, [(op, Expr)])
list :: Parser elem -> Parser sep -> Parser (elem, [(sep, elem)])
list elem sep = do
  first <- elem
  rest <- goParser <|> return []
  return (first, rest)
  where
    goParser = do
      sep' <- sep
      elem' <- elem
      res <- goParser <|> return []
      return $ (sep', elem') : res


char :: Char -> Parser Char
char c = Parser $ \str ->
  case str of
    (h : t) | c == h -> Just (t, c)
    _ -> Nothing

instance Functor Parser where
  fmap f p = Parser $ \str ->
    case runParser p str of
      Just (s', res) -> Just (s', f res)
      Nothing -> Nothing

instance Applicative Parser where
  pure x = Parser $ \str -> Just (str, x)

  (Parser l) <*> (Parser r) = Parser $ \str -> do
    (str', f) <- l str
    (str'', x) <- r str'
    return (str'', f x)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing

  l <|> r = Parser $ \str ->
    case runParser l str of
      Just (str', res) -> Just (str', res)
      Nothing -> runParser r str

instance Monad Parser where
  l >>= r = Parser $ \str ->
    case runParser l str of
      Just (str', res) -> runParser (r res) str'
      Nothing -> Nothing
