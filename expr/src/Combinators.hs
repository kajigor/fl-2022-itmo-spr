{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Combinators where

import Control.Applicative

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

-- first [(sep, second), (sep', third)] -> sep' (sep (first, second)) third

leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  foldl (\acc (sep, elem) -> f acc sep elem) first rest

rightAssoc :: (elem -> sep -> elem -> elem) -> ([(elem, sep)], elem) -> elem
rightAssoc f (rest, last) =
  foldr (\(elem, sep) acc -> f elem sep acc) last rest

listR :: Parser elem -> Parser sep -> Parser ([(elem, sep)], elem)
listR elem sep = do
  head <- goParser <|> return []
  last <- elem
  return (head, last)
  where
    goParser = do
      elem' <- elem
      sep' <- sep
      res <- goParser <|> return []
      return ((elem', sep') : res)

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
      return ((sep', elem') : res)

char :: Char -> Parser Char
char c = Parser $ \case
  (h : t) | c == h -> Just (t, c)
  _ -> Nothing

instance Functor Parser where
  fmap f p = Parser $ \str ->
    case runParser p str of
      Just (s', res) -> Just (s', f res)
      Nothing -> Nothing

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \s -> Just (s, x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser l) <*> (Parser r) = Parser $ \s -> case l s of
    Just (s', f) -> case r s' of
      Just (s'', x) -> Just (s'', f x)
      Nothing -> Nothing
    Nothing -> Nothing

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing 

  (<|>) :: Parser a -> Parser a -> Parser a
  l <|> r = Parser $ \str ->
    case runParser l str of
      Just (str', res) -> Just (str', res)
      Nothing -> runParser r str

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser l) >>= f = Parser $ \s -> case l s of
    Just (s', x) -> runParser (f x) s'
    Nothing -> Nothing
