{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Combinators where
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  foldl (\acc (sep, elem) -> f acc sep elem) first rest

rightAssoc :: (elem -> sep -> elem -> elem) -> ([(elem, sep)], elem) -> elem
rightAssoc f (first, rest) =
    foldr (\(elem, sep) acc -> f elem sep acc) rest first


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
                do {
                  res <- goParser;
                  return ((sep', elem'):res)
                } <|> return [(sep', elem')]

listR :: Parser elem -> Parser sep -> Parser ([(elem, sep)], elem)
listR elem sep = do
                  res <- goParser <|> return []
                  first <- elem
                  return (res, first)
  where goParser = do
                    elem' <- elem
                    sep' <- sep
                    do {
                      res <- goParser;
                      return ((elem', sep'):res)
                    } <|> return [(elem', sep')]

char :: Char -> Parser Char
char c = Parser $ \str ->
  case str of
    (h : t) | c == h -> Just (t, c)
    _ -> Nothing


instance Applicative Parser where
  pure a = Parser (\str -> Just (str, a))
  (Parser p1) <*> (Parser p2) = Parser $ \str -> case p1 str of
                                                  Nothing -> Nothing
                                                  Just (str', f) ->
                                                    case p2 str' of
                                                      Nothing -> Nothing
                                                      Just (str'', e) -> Just (str'', f e)

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser l) <|> (Parser r) = Parser $ \str -> case l str of
                                                Just (str', res) -> Just (str', res)
                                                Nothing -> r str

instance Functor Parser where
  fmap f (Parser p) = Parser $ \str ->
    case p str of
      Just (s', res) -> Just (s', f res)
      Nothing -> Nothing


instance Monad Parser where
  return = pure
  (Parser p1) >>= p2 = Parser $ \str -> case p1 str of
                                          Just (str', res) -> runParser (p2 res) str'
                                          Nothing -> Nothing
